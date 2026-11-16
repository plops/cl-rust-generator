import { OggPacket, OpusHead, OpusTags } from './types';

function readUint16LE(buf: Uint8Array, off: number): number {
  return buf[off] | (buf[off + 1] << 8);
}

function readInt16LE(buf: Uint8Array, off: number): number {
  const u = readUint16LE(buf, off);
  return u & 0x8000 ? u - 0x10000 : u;
}

function readUint32LE(buf: Uint8Array, off: number): number {
  return (
    (buf[off] |
      (buf[off + 1] << 8) |
      (buf[off + 2] << 16) |
      (buf[off + 3] << 24)) >>>
    0
  );
}

function readInt64LE(buf: Uint8Array, off: number): number {
  const lo = readUint32LE(buf, off);
  const hi = readUint32LE(buf, off + 4);
  if (lo === 0xffffffff && hi === 0xffffffff) return -1;
  return hi * 4294967296 + lo;
}

function decodeUtf8(bytes: Uint8Array): string {
  return new TextDecoder('utf-8', { fatal: false }).decode(bytes);
}

export function parseOpusHead(pkt: Uint8Array): OpusHead {
  if (pkt.length < 19) {
    throw new Error(`OpusHead zu kurz (${pkt.length} Bytes).`);
  }
  const magic = decodeUtf8(pkt.subarray(0, 8));
  if (magic !== 'OpusHead') {
    throw new Error(`Ungültiger OpusHead-Header ("${magic}").`);
  }
  const version = pkt[8];
  const channels = pkt[9];
  if (channels < 1 || channels > 2) {
    throw new Error(
      `Nur Mono (1 Kanal) und Stereo (2 Kanäle) werden unterstützt (gefunden: ${channels} Kanäle).`
    );
  }
  const preSkip = readUint16LE(pkt, 10);
  const inputSampleRate = readUint32LE(pkt, 12);
  const outputGainRaw = readInt16LE(pkt, 16);
  const outputGainDb = outputGainRaw / 256;
  const mappingFamily = pkt[18];

  return {
    version,
    channels,
    preSkip,
    inputSampleRate,
    outputGainDb,
    mappingFamily,
  };
}

export function parseOpusTags(pkt: Uint8Array): OpusTags {
  if (pkt.length < 16) {
    return { vendor: '', comments: [] };
  }
  const magic = decodeUtf8(pkt.subarray(0, 8));
  if (magic !== 'OpusTags') {
    return { vendor: '', comments: [] };
  }
  let pos = 8;
  const vendorLen = readUint32LE(pkt, pos);
  pos += 4;
  if (pos + vendorLen > pkt.length) {
    return { vendor: '', comments: [] };
  }
  const vendor = decodeUtf8(pkt.subarray(pos, pos + vendorLen));
  pos += vendorLen;

  const comments: Array<[string, string]> = [];
  if (pos + 4 <= pkt.length) {
    const count = readUint32LE(pkt, pos);
    pos += 4;
    for (let i = 0; i < count && pos + 4 <= pkt.length; i++) {
      const clen = readUint32LE(pkt, pos);
      pos += 4;
      if (pos + clen > pkt.length) break;
      const str = decodeUtf8(pkt.subarray(pos, pos + clen));
      pos += clen;
      const eq = str.indexOf('=');
      if (eq >= 0) {
        comments.push([str.slice(0, eq), str.slice(eq + 1)]);
      } else {
        comments.push([str, '']);
      }
    }
  }
  return { vendor, comments };
}

export interface OggDemuxResult {
  head: OpusHead;
  tags: OpusTags;
  packets: OggPacket[];
  serial: number;
}

export function demuxOggOpus(bytes: Uint8Array): OggDemuxResult {
  if (bytes.length < 27) {
    throw new Error('Datei zu kurz für einen gültigen Ogg-Container.');
  }

  let offset = 0;
  let targetSerial: number | null = null;
  let head: OpusHead | null = null;
  let tags: OpusTags | null = null;
  const audioPackets: OggPacket[] = [];

  let currentChunks: Uint8Array[] = [];
  let packetCounter = 0;
  let audioPacketIndex = 0;

  while (offset + 27 <= bytes.length) {
    if (
      bytes[offset] !== 0x4f || // O
      bytes[offset + 1] !== 0x67 || // g
      bytes[offset + 2] !== 0x67 || // g
      bytes[offset + 3] !== 0x53 // S
    ) {
      // Search forward for next OggS sync if slightly corrupted/padded
      let found = -1;
      for (let i = offset + 1; i + 27 <= bytes.length; i++) {
        if (
          bytes[i] === 0x4f &&
          bytes[i + 1] === 0x67 &&
          bytes[i + 2] === 0x67 &&
          bytes[i + 3] === 0x53
        ) {
          found = i;
          break;
        }
      }
      if (found < 0) break;
      offset = found;
    }

    const version = bytes[offset + 4];
    if (version !== 0) {
      throw new Error(`Nicht unterstützte Ogg-Seitenversion ${version}.`);
    }
    const headerType = bytes[offset + 5];
    const granulePos = readInt64LE(bytes, offset + 6);
    const serial = readUint32LE(bytes, offset + 14);
    const pageSeq = readUint32LE(bytes, offset + 18);
    const pageSegments = bytes[offset + 26];

    const segTableStart = offset + 27;
    const dataStart = segTableStart + pageSegments;
    if (dataStart > bytes.length) {
      throw new Error('Abgeschnittener Ogg-Seitentabellen-Header.');
    }

    let pageDataSize = 0;
    for (let i = 0; i < pageSegments; i++) {
      pageDataSize += bytes[segTableStart + i];
    }
    if (dataStart + pageDataSize > bytes.length) {
      throw new Error('Abgeschnittene Ogg-Seitendaten.');
    }

    const isContinued = (headerType & 0x01) !== 0;
    if (targetSerial === null) {
      // Check first packet for OpusHead
      let firstLen = 0;
      for (let i = 0; i < pageSegments; i++) {
        const l = bytes[segTableStart + i];
        firstLen += l;
        if (l < 255) break;
      }
      if (
        firstLen >= 8 &&
        decodeUtf8(bytes.subarray(dataStart, dataStart + 8)) === 'OpusHead'
      ) {
        targetSerial = serial;
      } else {
        offset = dataStart + pageDataSize;
        continue;
      }
    }

    if (serial !== targetSerial) {
      offset = dataStart + pageDataSize;
      continue;
    }

    if (!isContinued && currentChunks.length > 0) {
      // Previous incomplete packet discarded on discontinuity
      currentChunks = [];
    }

    let cursor = dataStart;
    for (let i = 0; i < pageSegments; i++) {
      const segLen = bytes[segTableStart + i];
      if (segLen > 0) {
        currentChunks.push(bytes.subarray(cursor, cursor + segLen));
        cursor += segLen;
      }
      if (segLen < 255) {
        // Packet complete
        let totalLen = 0;
        for (const c of currentChunks) totalLen += c.length;
        const pktData = new Uint8Array(totalLen);
        let p = 0;
        for (const c of currentChunks) {
          pktData.set(c, p);
          p += c.length;
        }
        currentChunks = [];

        if (packetCounter === 0) {
          head = parseOpusHead(pktData);
        } else if (packetCounter === 1) {
          tags = parseOpusTags(pktData);
        } else if (pktData.length > 0) {
          audioPackets.push({
            data: pktData,
            granulePos,
            pageSeq,
            packetIndex: audioPacketIndex++,
          });
        }
        packetCounter++;
      }
    }

    offset = dataStart + pageDataSize;
  }

  if (!head || targetSerial === null) {
    throw new Error('Kein gültiger Opus-Stream (OpusHead) in der Ogg-Datei gefunden.');
  }

  return {
    head,
    tags: tags ?? { vendor: '', comments: [] },
    packets: audioPackets,
    serial: targetSerial,
  };
}
