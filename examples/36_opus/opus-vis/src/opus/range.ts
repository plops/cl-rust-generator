// Bit-accurate RFC 6716 / libopus Range Decoder using 32-bit unsigned ops.

const EC_UINT_BITS = 8;
const EC_WINDOW_SIZE = 32;
const EC_SYM_BITS = 8;
const EC_CODE_BITS = 32;
const EC_SYM_MAX = (1 << EC_SYM_BITS) - 1;
const EC_CODE_TOP = 0x80000000;
const EC_CODE_BOT = EC_CODE_TOP >>> EC_SYM_BITS;
const EC_CODE_EXTRA = (EC_CODE_BITS - 2) % EC_SYM_BITS + 1; // 7
export const BITRES = 3;

const CORR = [35733, 38967, 42495, 46340, 50535, 55109, 60097, 65535];

function ilog32(v: number): number {
  let x = v >>> 0;
  let ret = 0;
  if (x >= 0x10000) {
    x >>>= 16;
    ret += 16;
  }
  if (x >= 0x100) {
    x >>>= 8;
    ret += 8;
  }
  if (x >= 0x10) {
    x >>>= 4;
    ret += 4;
  }
  if (x >= 0x4) {
    x >>>= 2;
    ret += 2;
  }
  if (x >= 0x2) {
    x >>>= 1;
    ret += 1;
  }
  if (x >= 0x1) {
    ret += 1;
  }
  return ret;
}

export class RangeDec {
  public readonly buf: Uint8Array;
  public storage: number;
  public endOffs: number;
  public endWindow: number;
  public nEndBits: number;
  public nbitsTotal: number;
  public offs: number;
  public rng: number;
  public val: number;
  public ext: number;
  public rem: number;
  public error: number;

  constructor(buf: Uint8Array) {
    this.buf = buf;
    this.storage = buf.length;
    this.endOffs = 0;
    this.endWindow = 0;
    this.nEndBits = 0;
    this.offs = 0;
    this.error = 0;
    this.rem = 0;
    this.ext = 0;
    this.nbitsTotal = EC_CODE_BITS + 1 - ((EC_CODE_BITS - EC_CODE_EXTRA) % EC_SYM_BITS); // 9
    this.rng = (1 << EC_CODE_EXTRA) >>> 0; // 128
    this.rem = this.readByte();
    this.val = (this.rng - 1 - (this.rem >>> (EC_SYM_BITS - EC_CODE_EXTRA))) >>> 0;
    this.normalize();
  }

  private readByte(): number {
    return this.offs < this.storage ? this.buf[this.offs++] : 0;
  }

  private readByteFromEnd(): number {
    return this.endOffs < this.storage
      ? this.buf[this.storage - 1 - this.endOffs++]
      : 0;
  }

  private normalize(): void {
    while (this.rng <= EC_CODE_BOT) {
      this.nbitsTotal += EC_SYM_BITS;
      this.rng = (this.rng << EC_SYM_BITS) >>> 0;
      let sym = this.rem;
      this.rem = this.readByte();
      sym = ((sym << EC_SYM_BITS) | this.rem) >>> (EC_SYM_BITS - EC_CODE_EXTRA);
      this.val = (((this.val << EC_SYM_BITS) + (EC_SYM_MAX & ~sym)) & (EC_CODE_TOP - 1)) >>> 0;
    }
  }

  public tell(): number {
    return this.nbitsTotal - ilog32(this.rng);
  }

  public tellFrac(): number {
    const nbits = this.nbitsTotal << BITRES;
    let l = ilog32(this.rng);
    let r = (this.rng >>> (l - 16)) >>> 0;
    let b = (r >> 12) - 8;
    b += r > CORR[b] ? 1 : 0;
    l = (l << BITRES) + b;
    return nbits - l;
  }

  public decode(ft: number): number {
    const ftU = ft >>> 0;
    this.ext = Math.floor(this.rng / ftU) >>> 0;
    const s = Math.floor(this.val / this.ext) >>> 0;
    const ret = ftU - Math.min(s + 1, ftU);
    return ret;
  }

  public decodeBin(ftb: number): number {
    const s = (this.rng >>> ftb) >>> 0;
    this.ext = s;
    const f = Math.floor(this.val / s) >>> 0;
    const ft = (1 << ftb) >>> 0;
    return ft - Math.min(f + 1, ft);
  }

  public update(fl: number, fh: number, ft: number): void {
    const s = Math.imul(this.ext, ft - fh) >>> 0;
    this.val = (this.val - s) >>> 0;
    if (fl > 0) {
      this.rng = Math.imul(this.ext, fh - fl) >>> 0;
    } else {
      this.rng = (this.rng - s) >>> 0;
    }
    this.normalize();
  }

  public decodeBitLogp(logp: number): number {
    const r = this.rng;
    const d = this.val;
    const s = r >>> logp;
    const ret = d < s ? 1 : 0;
    if (!ret) {
      this.val = (d - s) >>> 0;
    }
    this.rng = ret ? s : (r - s) >>> 0;
    this.normalize();
    return ret;
  }

  public decodeIcdf(icdf: readonly number[], ftb: number): number {
    let s = this.rng;
    const d = this.val;
    const r = s >>> ftb;
    let ret = -1;
    let t: number;
    do {
      t = s;
      ret++;
      s = Math.imul(r, icdf[ret]) >>> 0;
    } while (d < s);
    this.val = (d - s) >>> 0;
    this.rng = (t - s) >>> 0;
    this.normalize();
    return ret;
  }

  public decodeIcdf16(icdf: readonly number[], ftb: number): number {
    return this.decodeIcdf(icdf, ftb);
  }

  public decUint(ft: number): number {
    const ftU = ft >>> 0;
    if (ftU <= 1) return 0;
    const ft1 = ftU - 1;
    const ftb = ilog32(ft1);
    if (ftb > EC_UINT_BITS) {
      const shift = ftb - EC_UINT_BITS;
      const ftHead = (ft1 >>> shift) + 1;
      const s = this.decode(ftHead);
      this.update(s, s + 1, ftHead);
      const t = ((s << shift) | this.decBits(shift)) >>> 0;
      if (t >= ftU) {
        this.error = 1;
        return ft1;
      }
      return t;
    } else {
      const t = this.decode(ftU);
      this.update(t, t + 1, ftU);
      return t;
    }
  }

  public decBits(bits: number): number {
    if (bits === 0) return 0;
    let window = this.endWindow >>> 0;
    let available = this.nEndBits;
    if (available < bits) {
      do {
        window = (window | ((this.readByteFromEnd() << available) >>> 0)) >>> 0;
        available += EC_SYM_BITS;
      } while (available <= EC_WINDOW_SIZE - EC_SYM_BITS && available < bits);
    }
    const mask = bits === 32 ? 0xffffffff : ((1 << bits) - 1) >>> 0;
    const ret = (window & mask) >>> 0;
    window = window >>> bits;
    available -= bits;
    this.endWindow = window;
    this.nEndBits = available;
    this.nbitsTotal += bits;
    return ret;
  }

  public laplaceDecode(fs0: number, decay: number): number {
    let val = 0;
    const fm = this.decodeBin(15);
    let fl = 0;
    let fs = fs0;
    if (fm >= fs) {
      val++;
      fl = fs;
      fs = (((32768 - 32 - fs0) * (16384 - decay)) >> 15) + 1;
      while (fm >= fl + 2 * fs && fs > 1) {
        fl += 2 * fs;
        fs = (((2 * fs - 2) * decay) >> 15) + 1;
        val++;
      }
      if (fs <= 1) {
        const di = (fm - fl) >>> 1;
        val += di;
        fl += 2 * di;
        fs = 1;
      }
      if (fm < fl + fs) {
        val = -val;
      } else {
        fl += fs;
      }
    }
    const fh = fl + fs > 32768 ? 32768 : fl + fs;
    this.update(fl, fh, 32768);
    return val;
  }
}
