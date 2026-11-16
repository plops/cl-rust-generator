import { parseToc, TocInfo } from './toc';

export interface SplitPacket {
  toc: TocInfo;
  frames: Uint8Array[];
  vbr: boolean;
  paddingBytes: number;
}

function parseFrameLen(
  data: Uint8Array,
  pos: number
): { len: number; consumed: number } {
  if (pos >= data.length) {
    throw new Error('Paket abgeschnitten beim Lesen der Frame-Länge.');
  }
  const b0 = data[pos];
  if (b0 < 252) {
    return { len: b0, consumed: 1 };
  }
  if (pos + 1 >= data.length) {
    throw new Error('Paket abgeschnitten bei 2-Byte-Frame-Länge.');
  }
  const b1 = data[pos + 1];
  return { len: 4 * b1 + b0, consumed: 2 };
}

export function splitPacket(packet: Uint8Array): SplitPacket {
  if (packet.length < 1) {
    throw new Error('Leeres Opus-Paket.');
  }

  const toc = parseToc(packet[0]);
  const code = toc.code;

  if (code === 0) {
    return {
      toc,
      frames: [packet.subarray(1)],
      vbr: false,
      paddingBytes: 0,
    };
  }

  if (code === 1) {
    const payloadLen = packet.length - 1;
    if ((payloadLen & 1) !== 0) {
      throw new Error(`Code 1 Paket mit ungerader Länge (${payloadLen}).`);
    }
    const half = payloadLen >> 1;
    return {
      toc,
      frames: [packet.subarray(1, 1 + half), packet.subarray(1 + half)],
      vbr: false,
      paddingBytes: 0,
    };
  }

  if (code === 2) {
    const { len, consumed } = parseFrameLen(packet, 1);
    const start0 = 1 + consumed;
    if (start0 + len > packet.length) {
      throw new Error('Code 2 Frame 0 überschreitet Paketgrenze.');
    }
    return {
      toc,
      frames: [
        packet.subarray(start0, start0 + len),
        packet.subarray(start0 + len),
      ],
      vbr: true,
      paddingBytes: 0,
    };
  }

  // Code 3: arbitrary frame count, optional VBR & padding
  if (packet.length < 2) {
    throw new Error('Code 3 Paket zu kurz.');
  }
  const countByte = packet[1];
  const vbr = (countByte & 0x80) !== 0;
  const hasPad = (countByte & 0x40) !== 0;
  const frameCount = countByte & 0x3f;

  if (frameCount === 0 || frameCount * toc.samplesPerFrame > 5760) {
    throw new Error(`Ungültige Frame-Anzahl (${frameCount}) in Code 3 Paket.`);
  }

  let pos = 2;
  let paddingBytes = 0;

  if (hasPad) {
    let totalPad = 0;
    while (true) {
      if (pos >= packet.length) {
        throw new Error('Padding überschreitet Paketgrenze.');
      }
      const p = packet[pos++];
      if (p === 255) {
        totalPad += 254;
      } else {
        totalPad += p;
        break;
      }
    }
    paddingBytes = totalPad;
  }

  const payloadEnd = packet.length - paddingBytes;
  if (payloadEnd < pos) {
    throw new Error('Padding größer als restliches Paket.');
  }

  const frames: Uint8Array[] = [];
  if (vbr) {
    const lens: number[] = [];
    let sumLens = 0;
    for (let i = 0; i < frameCount - 1; i++) {
      const { len, consumed } = parseFrameLen(packet, pos);
      pos += consumed;
      lens.push(len);
      sumLens += len;
    }
    const lastLen = payloadEnd - pos - sumLens;
    if (lastLen < 0) {
      throw new Error('Code 3 VBR Längen überschreiten Nutzdaten.');
    }
    lens.push(lastLen);

    let cursor = pos;
    for (const l of lens) {
      frames.push(packet.subarray(cursor, cursor + l));
      cursor += l;
    }
  } else {
    const rem = payloadEnd - pos;
    if (rem % frameCount !== 0) {
      throw new Error(
        `Code 3 CBR Rest (${rem}) nicht durch Frame-Anzahl (${frameCount}) teilbar.`
      );
    }
    const each = rem / frameCount;
    for (let i = 0; i < frameCount; i++) {
      frames.push(packet.subarray(pos + i * each, pos + (i + 1) * each));
    }
  }

  return {
    toc,
    frames,
    vbr,
    paddingBytes,
  };
}
