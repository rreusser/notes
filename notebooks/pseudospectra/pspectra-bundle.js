var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
  mod
));

// lib/blas/base/dscal/lib/base.js
var require_base = __commonJS({
  "lib/blas/base/dscal/lib/base.js"(exports, module) {
    "use strict";
    var M = 5;
    function dscal(N, da, x, strideX, offsetX) {
      var ix;
      var m;
      var i;
      if (N <= 0) {
        return x;
      }
      ix = offsetX;
      if (strideX === 1) {
        m = N % M;
        if (m > 0) {
          for (i = 0; i < m; i++) {
            x[ix] *= da;
            ix += 1;
          }
        }
        if (N < M) {
          return x;
        }
        for (i = m; i < N; i += M) {
          x[ix] *= da;
          x[ix + 1] *= da;
          x[ix + 2] *= da;
          x[ix + 3] *= da;
          x[ix + 4] *= da;
          ix += M;
        }
        return x;
      }
      for (i = 0; i < N; i++) {
        x[ix] *= da;
        ix += strideX;
      }
      return x;
    }
    module.exports = dscal;
  }
});

// lib/blas/base/dnrm2/lib/base.js
var require_base2 = __commonJS({
  "lib/blas/base/dnrm2/lib/base.js"(exports, module) {
    "use strict";
    var TSML = 14916681462400413e-170;
    var TBIG = 1997919072202235e131;
    var SSML = 44989137945431964e145;
    var SBIG = 11113793747425387e-178;
    function dnrm2(N, x, stride, offset) {
      var notbig;
      var sumsq;
      var abig;
      var amed;
      var asml;
      var ymin;
      var ymax;
      var scl;
      var ax;
      var ix;
      var i;
      if (N <= 0) {
        return 0;
      }
      scl = 1;
      sumsq = 0;
      notbig = true;
      asml = 0;
      amed = 0;
      abig = 0;
      ix = offset;
      for (i = 0; i < N; i++) {
        ax = Math.abs(x[ix]);
        if (ax > TBIG) {
          abig += ax * SBIG * (ax * SBIG);
          notbig = false;
        } else if (ax < TSML) {
          if (notbig) {
            asml += ax * SSML * (ax * SSML);
          }
        } else {
          amed += ax * ax;
        }
        ix += stride;
      }
      if (abig > 0) {
        if (amed > 0 || amed !== amed) {
          abig += amed * SBIG * SBIG;
        }
        scl = 1 / SBIG;
        sumsq = abig;
      } else if (asml > 0) {
        if (amed > 0 || amed !== amed) {
          amed = Math.sqrt(amed);
          asml = Math.sqrt(asml) / SSML;
          if (asml > amed) {
            ymin = amed;
            ymax = asml;
          } else {
            ymin = asml;
            ymax = amed;
          }
          scl = 1;
          sumsq = ymax * ymax * (1 + ymin / ymax * (ymin / ymax));
        } else {
          scl = 1 / SSML;
          sumsq = asml;
        }
      } else {
        scl = 1;
        sumsq = amed;
      }
      return scl * Math.sqrt(sumsq);
    }
    module.exports = dnrm2;
  }
});

// lib/blas/base/idamax/lib/base.js
var require_base3 = __commonJS({
  "lib/blas/base/idamax/lib/base.js"(exports, module) {
    "use strict";
    function idamax(N, x, strideX, offsetX) {
      var dmax;
      var imax;
      var ix;
      var i;
      if (N < 1 || strideX <= 0) {
        return -1;
      }
      if (N === 1) {
        return 0;
      }
      ix = offsetX;
      dmax = Math.abs(x[ix]);
      imax = 0;
      ix += strideX;
      for (i = 1; i < N; i++) {
        if (Math.abs(x[ix]) > dmax) {
          imax = i;
          dmax = Math.abs(x[ix]);
        }
        ix += strideX;
      }
      return imax;
    }
    module.exports = idamax;
  }
});

// lib/blas/base/drot/lib/base.js
var require_base4 = __commonJS({
  "lib/blas/base/drot/lib/base.js"(exports, module) {
    "use strict";
    function drot(N, x, strideX, offsetX, y, strideY, offsetY, c, s) {
      var temp;
      var ix;
      var iy;
      var i;
      if (N <= 0) {
        return y;
      }
      ix = offsetX;
      iy = offsetY;
      for (i = 0; i < N; i++) {
        temp = c * x[ix] + s * y[iy];
        y[iy] = c * y[iy] - s * x[ix];
        x[ix] = temp;
        ix += strideX;
        iy += strideY;
      }
      return y;
    }
    module.exports = drot;
  }
});

// lib/lapack/base/dlamch/lib/base.js
var require_base5 = __commonJS({
  "lib/lapack/base/dlamch/lib/base.js"(exports, module) {
    "use strict";
    var EPS = 11102230246251565e-32;
    var SFMIN = 22250738585072014e-324;
    var BASE = 2;
    var PREC = EPS * BASE;
    var DIGITS = 53;
    var RND = 1;
    var EMIN = -1021;
    var RMIN = 22250738585072014e-324;
    var EMAX = 1024;
    var RMAX = 17976931348623157e292;
    var TABLE = {
      "epsilon": EPS,
      "Epsilon": EPS,
      "safe-minimum": SFMIN,
      "Safe minimum": SFMIN,
      "base": BASE,
      "Base": BASE,
      "precision": PREC,
      "Precision": PREC,
      "digits": DIGITS,
      "rounding": RND,
      "min-exponent": EMIN,
      "underflow": RMIN,
      "max-exponent": EMAX,
      "overflow": RMAX,
      "scale": SFMIN,
      "E": EPS,
      "e": EPS,
      "S": SFMIN,
      "s": SFMIN,
      "B": BASE,
      "b": BASE,
      "P": PREC,
      "p": PREC,
      "N": DIGITS,
      "n": DIGITS,
      "R": RND,
      "r": RND,
      "M": EMIN,
      "m": EMIN,
      "U": RMIN,
      "u": RMIN,
      "L": EMAX,
      "l": EMAX,
      "O": RMAX,
      "o": RMAX
    };
    function dlamch(cmach) {
      var v = TABLE[cmach];
      if (v !== void 0) {
        return v;
      }
      return 0;
    }
    module.exports = dlamch;
  }
});

// lib/lapack/base/dlassq/lib/base.js
var require_base6 = __commonJS({
  "lib/lapack/base/dlassq/lib/base.js"(exports, module) {
    "use strict";
    var TSML = Math.pow(2, -511);
    var TBIG = Math.pow(2, 486);
    var SSML = Math.pow(2, 537);
    var SBIG = Math.pow(2, -538);
    function dlassq(N, x, stride, offset, scale, sumsq) {
      var notbig;
      var abig;
      var amed;
      var asml;
      var ymax;
      var ymin;
      var ax;
      var ix;
      var i;
      if (scale !== scale || sumsq !== sumsq) {
        return {
          "scl": scale,
          "sumsq": sumsq
        };
      }
      if (sumsq === 0) {
        scale = 1;
      }
      if (scale === 0) {
        scale = 1;
        sumsq = 0;
      }
      if (N <= 0) {
        return {
          "scl": scale,
          "sumsq": sumsq
        };
      }
      notbig = true;
      asml = 0;
      amed = 0;
      abig = 0;
      ix = offset;
      if (stride < 0) {
        ix = offset - (N - 1) * stride;
      }
      for (i = 0; i < N; i++) {
        ax = Math.abs(x[ix]);
        if (ax > TBIG) {
          abig += ax * SBIG * (ax * SBIG);
          notbig = false;
        } else if (ax < TSML) {
          if (notbig) {
            asml += ax * SSML * (ax * SSML);
          }
        } else {
          amed += ax * ax;
        }
        ix += stride;
      }
      if (sumsq > 0) {
        ax = scale * Math.sqrt(sumsq);
        if (ax > TBIG) {
          if (scale > 1) {
            scale *= SBIG;
            abig += scale * (scale * sumsq);
          } else {
            abig += scale * (scale * (SBIG * (SBIG * sumsq)));
          }
        } else if (ax < TSML) {
          if (notbig) {
            if (scale < 1) {
              scale *= SSML;
              asml += scale * (scale * sumsq);
            } else {
              asml += scale * (scale * (SSML * (SSML * sumsq)));
            }
          }
        } else {
          amed += scale * (scale * sumsq);
        }
      }
      if (abig > 0) {
        if (amed > 0 || amed !== amed) {
          abig += amed * SBIG * SBIG;
        }
        scale = 1 / SBIG;
        sumsq = abig;
      } else if (asml > 0) {
        if (amed > 0 || amed !== amed) {
          amed = Math.sqrt(amed);
          asml = Math.sqrt(asml) / SSML;
          if (asml > amed) {
            ymin = amed;
            ymax = asml;
          } else {
            ymin = asml;
            ymax = amed;
          }
          scale = 1;
          sumsq = ymax * ymax * (1 + ymin / ymax * (ymin / ymax));
        } else {
          scale = 1 / SSML;
          sumsq = asml;
        }
      } else {
        scale = 1;
        sumsq = amed;
      }
      return {
        "scl": scale,
        "sumsq": sumsq
      };
    }
    module.exports = dlassq;
  }
});

// lib/lapack/base/dlange/lib/base.js
var require_base7 = __commonJS({
  "lib/lapack/base/dlange/lib/base.js"(exports, module) {
    "use strict";
    var dlassq = require_base6();
    function dlange(norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK) {
      var value;
      var scale;
      var temp;
      var sum;
      var out;
      var ai;
      var wi;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return 0;
      }
      if (norm === "max") {
        value = 0;
        for (j = 0; j < N; j++) {
          ai = offsetA + j * strideA2;
          for (i = 0; i < M; i++) {
            temp = Math.abs(A[ai]);
            if (value < temp || temp !== temp) {
              value = temp;
            }
            ai += strideA1;
          }
        }
      } else if (norm === "one-norm") {
        value = 0;
        for (j = 0; j < N; j++) {
          sum = 0;
          ai = offsetA + j * strideA2;
          for (i = 0; i < M; i++) {
            sum += Math.abs(A[ai]);
            ai += strideA1;
          }
          if (value < sum || sum !== sum) {
            value = sum;
          }
        }
      } else if (norm === "inf-norm") {
        for (i = 0; i < M; i++) {
          wi = offsetWORK + i * strideWORK;
          WORK[wi] = 0;
        }
        for (j = 0; j < N; j++) {
          ai = offsetA + j * strideA2;
          wi = offsetWORK;
          for (i = 0; i < M; i++) {
            WORK[wi] += Math.abs(A[ai]);
            ai += strideA1;
            wi += strideWORK;
          }
        }
        value = 0;
        for (i = 0; i < M; i++) {
          wi = offsetWORK + i * strideWORK;
          temp = WORK[wi];
          if (value < temp || temp !== temp) {
            value = temp;
          }
        }
      } else if (norm === "frobenius") {
        scale = 0;
        sum = 1;
        for (j = 0; j < N; j++) {
          out = dlassq(M, A, strideA1, offsetA + j * strideA2, scale, sum);
          scale = out.scl;
          sum = out.sumsq;
        }
        value = scale * Math.sqrt(sum);
      } else {
        value = 0;
      }
      return value;
    }
    module.exports = dlange;
  }
});

// lib/lapack/base/dlascl/lib/base.js
var require_base8 = __commonJS({
  "lib/lapack/base/dlascl/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    function dlascl(type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA) {
      var smlnum;
      var bignum;
      var cfromc;
      var cfrom1;
      var itype;
      var ctoc;
      var cto1;
      var done;
      var iMax;
      var iMin;
      var mul;
      var k1;
      var k2;
      var k3;
      var k4;
      var ai;
      var i;
      var j;
      if (type === "general") {
        itype = 0;
      } else if (type === "lower") {
        itype = 1;
      } else if (type === "upper") {
        itype = 2;
      } else if (type === "upper-hessenberg") {
        itype = 3;
      } else if (type === "lower-band") {
        itype = 4;
      } else if (type === "upper-band") {
        itype = 5;
      } else if (type === "band") {
        itype = 6;
      } else {
        return -1;
      }
      if (N === 0 || M === 0) {
        return 0;
      }
      smlnum = dlamch("safe-minimum");
      bignum = 1 / smlnum;
      cfromc = cfrom;
      ctoc = cto;
      done = false;
      while (!done) {
        cfrom1 = cfromc * smlnum;
        if (cfrom1 === cfromc) {
          mul = ctoc / cfromc;
          done = true;
        } else {
          cto1 = ctoc / bignum;
          if (cto1 === ctoc) {
            mul = ctoc;
            done = true;
            cfromc = 1;
          } else if (Math.abs(cfrom1) > Math.abs(ctoc) && ctoc !== 0) {
            mul = smlnum;
            done = false;
            cfromc = cfrom1;
          } else if (Math.abs(cto1) > Math.abs(cfromc)) {
            mul = bignum;
            done = false;
            ctoc = cto1;
          } else {
            mul = ctoc / cfromc;
            done = true;
            if (mul === 1) {
              return 0;
            }
          }
        }
        if (itype === 0) {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 1) {
          for (j = 0; j < N; j++) {
            for (i = j; i < M; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 2) {
          for (j = 0; j < N; j++) {
            iMax = Math.min(j + 1, M);
            for (i = 0; i < iMax; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 3) {
          for (j = 0; j < N; j++) {
            iMax = Math.min(j + 2, M);
            for (i = 0; i < iMax; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 4) {
          k3 = kl + 1;
          k4 = N + 1;
          for (j = 0; j < N; j++) {
            iMax = Math.min(k3, k4 - j - 1);
            for (i = 0; i < iMax; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 5) {
          k1 = ku + 2;
          k3 = ku + 1;
          for (j = 0; j < N; j++) {
            iMin = Math.max(k1 - j - 2, 0);
            for (i = iMin; i < k3; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        } else if (itype === 6) {
          k1 = kl + ku + 2;
          k2 = kl + 1;
          k3 = 2 * kl + ku + 1;
          k4 = kl + ku + 1 + M;
          for (j = 0; j < N; j++) {
            iMin = Math.max(k1 - j - 2, k2 - 1);
            iMax = Math.min(k3, k4 - j - 1);
            for (i = iMin; i < iMax; i++) {
              ai = offsetA + i * strideA1 + j * strideA2;
              A[ai] *= mul;
            }
          }
        }
      }
      return 0;
    }
    module.exports = dlascl;
  }
});

// lib/lapack/base/dlacpy/lib/base.js
var require_base9 = __commonJS({
  "lib/lapack/base/dlacpy/lib/base.js"(exports, module) {
    "use strict";
    function dlacpy(uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB) {
      var da0;
      var db0;
      var i;
      var j;
      if (uplo === "upper") {
        for (j = 0; j < N; j++) {
          da0 = offsetA + j * strideA2;
          db0 = offsetB + j * strideB2;
          for (i = 0; i <= j && i < M; i++) {
            B[db0 + i * strideB1] = A[da0 + i * strideA1];
          }
        }
      } else if (uplo === "lower") {
        for (j = 0; j < N; j++) {
          da0 = offsetA + j * strideA2;
          db0 = offsetB + j * strideB2;
          for (i = j; i < M; i++) {
            B[db0 + i * strideB1] = A[da0 + i * strideA1];
          }
        }
      } else {
        for (j = 0; j < N; j++) {
          da0 = offsetA + j * strideA2;
          db0 = offsetB + j * strideB2;
          for (i = 0; i < M; i++) {
            B[db0 + i * strideB1] = A[da0 + i * strideA1];
          }
        }
      }
      return B;
    }
    module.exports = dlacpy;
  }
});

// lib/blas/base/dswap/lib/base.js
var require_base10 = __commonJS({
  "lib/blas/base/dswap/lib/base.js"(exports, module) {
    "use strict";
    function dswap(N, x, strideX, offsetX, y, strideY, offsetY) {
      var tmp;
      var ix;
      var iy;
      var i;
      if (N <= 0) {
        return y;
      }
      ix = offsetX;
      iy = offsetY;
      for (i = 0; i < N; i++) {
        tmp = x[ix];
        x[ix] = y[iy];
        y[iy] = tmp;
        ix += strideX;
        iy += strideY;
      }
      return y;
    }
    module.exports = dswap;
  }
});

// lib/lapack/base/dgebal/lib/base.js
var require_base11 = __commonJS({
  "lib/lapack/base/dgebal/lib/base.js"(exports, module) {
    "use strict";
    var dswap = require_base10();
    var dscal = require_base();
    var dnrm2 = require_base2();
    var idamax = require_base3();
    var dlamch = require_base5();
    var ZERO = 0;
    var ONE = 1;
    var SCLFAC = 2;
    var FACTOR = 0.95;
    var SFMIN1 = dlamch("safe-minimum") / dlamch("precision");
    var SFMAX1 = ONE / SFMIN1;
    var SFMIN2 = SFMIN1 * SCLFAC;
    var SFMAX2 = ONE / SFMIN2;
    function dgebal(job, N, A, strideA1, strideA2, offsetA, SCALE, strideSCALE, offsetSCALE) {
      var noconv;
      var canswap;
      var ca;
      var ra;
      var ica;
      var ira;
      var oA;
      var sA1;
      var sA2;
      var oS;
      var sS;
      var c;
      var f;
      var g;
      var r;
      var s;
      var i;
      var j;
      var k;
      var l;
      oA = offsetA;
      sA1 = strideA1;
      sA2 = strideA2;
      oS = offsetSCALE;
      sS = strideSCALE;
      if (N === 0) {
        return { "info": 0, "ilo": 1, "ihi": 0 };
      }
      if (job === "none") {
        for (i = 0; i < N; i++) {
          SCALE[oS + i * sS] = ONE;
        }
        return { "info": 0, "ilo": 1, "ihi": N };
      }
      k = 1;
      l = N;
      if (job !== "scale") {
        noconv = true;
        while (noconv) {
          noconv = false;
          for (i = l; i >= 1; i--) {
            canswap = true;
            for (j = 1; j <= l; j++) {
              if (i !== j && A[oA + (i - 1) * sA1 + (j - 1) * sA2] !== ZERO) {
                canswap = false;
                break;
              }
            }
            if (canswap) {
              SCALE[oS + (l - 1) * sS] = i;
              if (i !== l) {
                dswap(l, A, sA1, oA + (i - 1) * sA2, A, sA1, oA + (l - 1) * sA2);
                dswap(N - k + 1, A, sA2, oA + (i - 1) * sA1 + (k - 1) * sA2, A, sA2, oA + (l - 1) * sA1 + (k - 1) * sA2);
              }
              noconv = true;
              if (l === 1) {
                return { "info": 0, "ilo": 1, "ihi": 1 };
              }
              l -= 1;
            }
          }
        }
        noconv = true;
        while (noconv) {
          noconv = false;
          for (j = k; j <= l; j++) {
            canswap = true;
            for (i = k; i <= l; i++) {
              if (i !== j && A[oA + (i - 1) * sA1 + (j - 1) * sA2] !== ZERO) {
                canswap = false;
                break;
              }
            }
            if (canswap) {
              SCALE[oS + (k - 1) * sS] = j;
              if (j !== k) {
                dswap(l, A, sA1, oA + (j - 1) * sA2, A, sA1, oA + (k - 1) * sA2);
                dswap(N - k + 1, A, sA2, oA + (j - 1) * sA1 + (k - 1) * sA2, A, sA2, oA + (k - 1) * sA1 + (k - 1) * sA2);
              }
              noconv = true;
              k += 1;
            }
          }
        }
      }
      for (i = k; i <= l; i++) {
        SCALE[oS + (i - 1) * sS] = ONE;
      }
      if (job === "permute") {
        return { "info": 0, "ilo": k, "ihi": l };
      }
      noconv = true;
      while (noconv) {
        noconv = false;
        for (i = k; i <= l; i++) {
          c = dnrm2(l - k + 1, A, sA1, oA + (k - 1) * sA1 + (i - 1) * sA2);
          r = dnrm2(l - k + 1, A, sA2, oA + (i - 1) * sA1 + (k - 1) * sA2);
          ica = idamax(l, A, sA1, oA + (i - 1) * sA2);
          ca = Math.abs(A[oA + ica * sA1 + (i - 1) * sA2]);
          ira = idamax(N - k + 1, A, sA2, oA + (i - 1) * sA1 + (k - 1) * sA2);
          ra = Math.abs(A[oA + (i - 1) * sA1 + (ira + k - 1) * sA2]);
          if (c === ZERO || r === ZERO) {
            continue;
          }
          if (c + ca + r + ra !== c + ca + r + ra) {
            return { "info": -3, "ilo": k, "ihi": l };
          }
          g = r / SCLFAC;
          f = ONE;
          s = c + r;
          while (c < g && Math.max(f, c, ca) < SFMAX2 && Math.min(r, g, ra) > SFMIN2) {
            f *= SCLFAC;
            c *= SCLFAC;
            ca *= SCLFAC;
            r /= SCLFAC;
            g /= SCLFAC;
            ra /= SCLFAC;
          }
          g = c / SCLFAC;
          while (g >= r && Math.max(r, ra) < SFMAX2 && Math.min(f, c, g, ca) > SFMIN2) {
            f /= SCLFAC;
            c /= SCLFAC;
            g /= SCLFAC;
            ca /= SCLFAC;
            r *= SCLFAC;
            ra *= SCLFAC;
          }
          if (c + r >= FACTOR * s) {
            continue;
          }
          if (f < ONE && SCALE[oS + (i - 1) * sS] < ONE) {
            if (f * SCALE[oS + (i - 1) * sS] <= SFMIN1) {
              continue;
            }
          }
          if (f > ONE && SCALE[oS + (i - 1) * sS] > ONE) {
            if (SCALE[oS + (i - 1) * sS] >= SFMAX1 / f) {
              continue;
            }
          }
          g = ONE / f;
          SCALE[oS + (i - 1) * sS] *= f;
          noconv = true;
          dscal(N - k + 1, g, A, sA2, oA + (i - 1) * sA1 + (k - 1) * sA2);
          dscal(l, f, A, sA1, oA + (i - 1) * sA2);
        }
      }
      return { "info": 0, "ilo": k, "ihi": l };
    }
    module.exports = dgebal;
  }
});

// lib/lapack/base/dgebak/lib/base.js
var require_base12 = __commonJS({
  "lib/lapack/base/dgebak/lib/base.js"(exports, module) {
    "use strict";
    var dscal = require_base();
    var dswap = require_base10();
    function dgebak(job, side, N, ilo, ihi, SCALE, strideSCALE, offsetSCALE, M, V, strideV1, strideV2, offsetV) {
      var rightv;
      var leftv;
      var ii;
      var s;
      var i;
      var k;
      rightv = side === "right";
      leftv = side === "left";
      if (N === 0 || M === 0 || job === "none") {
        return 0;
      }
      var ilo0 = ilo - 1;
      var ihi0 = ihi - 1;
      if (ilo0 !== ihi0) {
        if (job === "scale" || job === "both") {
          if (rightv) {
            for (i = ilo0; i <= ihi0; i++) {
              s = SCALE[offsetSCALE + i * strideSCALE];
              dscal(M, s, V, strideV2, offsetV + i * strideV1);
            }
          }
          if (leftv) {
            for (i = ilo0; i <= ihi0; i++) {
              s = 1 / SCALE[offsetSCALE + i * strideSCALE];
              dscal(M, s, V, strideV2, offsetV + i * strideV1);
            }
          }
        }
      }
      if (job === "permute" || job === "both") {
        if (rightv) {
          for (ii = 0; ii < N; ii++) {
            i = ii;
            if (i >= ilo0 && i <= ihi0) {
              continue;
            }
            if (i < ilo0) {
              i = ilo0 - ii - 1;
            }
            k = SCALE[offsetSCALE + i * strideSCALE] | 0;
            k = k - 1;
            if (k === i) {
              continue;
            }
            dswap(M, V, strideV2, offsetV + i * strideV1, V, strideV2, offsetV + k * strideV1);
          }
        }
        if (leftv) {
          for (ii = 0; ii < N; ii++) {
            i = ii;
            if (i >= ilo0 && i <= ihi0) {
              continue;
            }
            if (i < ilo0) {
              i = ilo0 - ii - 1;
            }
            k = SCALE[offsetSCALE + i * strideSCALE] | 0;
            k = k - 1;
            if (k === i) {
              continue;
            }
            dswap(M, V, strideV2, offsetV + i * strideV1, V, strideV2, offsetV + k * strideV1);
          }
        }
      }
      return 0;
    }
    module.exports = dgebak;
  }
});

// lib/blas/base/daxpy/lib/base.js
var require_base13 = __commonJS({
  "lib/blas/base/daxpy/lib/base.js"(exports, module) {
    "use strict";
    var M = 4;
    function daxpy(N, alpha, x, strideX, offsetX, y, strideY, offsetY) {
      var ix;
      var iy;
      var m;
      var i;
      if (N <= 0) {
        return y;
      }
      if (alpha === 0) {
        return y;
      }
      ix = offsetX;
      iy = offsetY;
      if (strideX === 1 && strideY === 1) {
        m = N % M;
        if (m > 0) {
          for (i = 0; i < m; i++) {
            y[iy] += alpha * x[ix];
            ix += 1;
            iy += 1;
          }
        }
        if (N < M) {
          return y;
        }
        for (i = m; i < N; i += M) {
          y[iy] += alpha * x[ix];
          y[iy + 1] += alpha * x[ix + 1];
          y[iy + 2] += alpha * x[ix + 2];
          y[iy + 3] += alpha * x[ix + 3];
          ix += M;
          iy += M;
        }
        return y;
      }
      for (i = 0; i < N; i++) {
        y[iy] += alpha * x[ix];
        ix += strideX;
        iy += strideY;
      }
      return y;
    }
    module.exports = daxpy;
  }
});

// lib/blas/base/dgemv/lib/base.js
var require_base14 = __commonJS({
  "lib/blas/base/dgemv/lib/base.js"(exports, module) {
    "use strict";
    function dgemv(trans, M, N, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY) {
      var noTrans;
      var temp;
      var leny;
      var sa1;
      var sa2;
      var ia;
      var ix;
      var iy;
      var jx;
      var jy;
      var i;
      var j;
      noTrans = trans === "no-transpose";
      if (M === 0 || N === 0 || alpha === 0 && beta === 1) {
        return y;
      }
      sa1 = strideA1;
      sa2 = strideA2;
      if (noTrans) {
        leny = M;
      } else {
        leny = N;
      }
      if (beta !== 1) {
        iy = offsetY;
        if (beta === 0) {
          for (i = 0; i < leny; i++) {
            y[iy] = 0;
            iy += strideY;
          }
        } else {
          for (i = 0; i < leny; i++) {
            y[iy] *= beta;
            iy += strideY;
          }
        }
      }
      if (alpha === 0) {
        return y;
      }
      if (noTrans) {
        jx = offsetX;
        for (j = 0; j < N; j++) {
          temp = alpha * x[jx];
          iy = offsetY;
          ia = offsetA + j * sa2;
          for (i = 0; i < M; i++) {
            y[iy] += temp * A[ia];
            iy += strideY;
            ia += sa1;
          }
          jx += strideX;
        }
      } else {
        jy = offsetY;
        for (j = 0; j < N; j++) {
          temp = 0;
          ix = offsetX;
          ia = offsetA + j * sa2;
          for (i = 0; i < M; i++) {
            temp += A[ia] * x[ix];
            ix += strideX;
            ia += sa1;
          }
          y[jy] += alpha * temp;
          jy += strideY;
        }
      }
      return y;
    }
    module.exports = dgemv;
  }
});

// lib/blas/base/dger/lib/base.js
var require_base15 = __commonJS({
  "lib/blas/base/dger/lib/base.js"(exports, module) {
    "use strict";
    function dger(M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA) {
      var temp;
      var ix;
      var jy;
      var i;
      var j;
      if (M === 0 || N === 0 || alpha === 0) {
        return A;
      }
      jy = offsetY;
      for (j = 0; j < N; j++) {
        if (y[jy] !== 0) {
          temp = alpha * y[jy];
          ix = offsetX;
          for (i = 0; i < M; i++) {
            A[offsetA + i * strideA1 + j * strideA2] += x[ix] * temp;
            ix += strideX;
          }
        }
        jy += strideY;
      }
      return A;
    }
    module.exports = dger;
  }
});

// lib/lapack/base/iladlr/lib/base.js
var require_base16 = __commonJS({
  "lib/lapack/base/iladlr/lib/base.js"(exports, module) {
    "use strict";
    function iladlr(M, N, A, strideA1, strideA2, offsetA) {
      var result;
      var i;
      var j;
      if (M === 0) {
        return -1;
      }
      if (A[offsetA + (M - 1) * strideA1] !== 0 || A[offsetA + (M - 1) * strideA1 + (N - 1) * strideA2] !== 0) {
        return M - 1;
      }
      result = -1;
      for (j = 0; j < N; j++) {
        i = M - 1;
        while (i >= 0 && A[offsetA + i * strideA1 + j * strideA2] === 0) {
          i -= 1;
        }
        if (i > result) {
          result = i;
        }
      }
      return result;
    }
    module.exports = iladlr;
  }
});

// lib/lapack/base/iladlc/lib/base.js
var require_base17 = __commonJS({
  "lib/lapack/base/iladlc/lib/base.js"(exports, module) {
    "use strict";
    function iladlc(M, N, A, strideA1, strideA2, offsetA) {
      var i;
      var j;
      if (N === 0) {
        return -1;
      }
      if (A[offsetA + (N - 1) * strideA2] !== 0 || A[offsetA + (M - 1) * strideA1 + (N - 1) * strideA2] !== 0) {
        return N - 1;
      }
      for (j = N - 1; j >= 0; j--) {
        for (i = 0; i < M; i++) {
          if (A[offsetA + i * strideA1 + j * strideA2] !== 0) {
            return j;
          }
        }
      }
      return -1;
    }
    module.exports = iladlc;
  }
});

// lib/lapack/base/dlarf/lib/base.js
var require_base18 = __commonJS({
  "lib/lapack/base/dlarf/lib/base.js"(exports, module) {
    "use strict";
    var dgemv = require_base14();
    var dger = require_base15();
    var iladlr = require_base16();
    var iladlc = require_base17();
    function dlarf(side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var applyLeft;
      var lastv;
      var lastc;
      var ix;
      applyLeft = side === "left";
      lastv = 0;
      lastc = 0;
      if (tau !== 0) {
        if (applyLeft) {
          lastv = M;
        } else {
          lastv = N;
        }
        if (strideV > 0) {
          ix = offsetV + (lastv - 1) * strideV;
        } else {
          ix = offsetV;
        }
        while (lastv > 0 && v[ix] === 0) {
          lastv -= 1;
          ix -= strideV;
        }
        if (applyLeft) {
          lastc = iladlc(lastv, N, C, strideC1, strideC2, offsetC) + 1;
        } else {
          lastc = iladlr(M, lastv, C, strideC1, strideC2, offsetC) + 1;
        }
      }
      if (applyLeft) {
        if (lastv > 0) {
          dgemv(
            "transpose",
            lastv,
            lastc,
            1,
            C,
            strideC1,
            strideC2,
            offsetC,
            v,
            strideV,
            offsetV,
            0,
            WORK,
            strideWORK,
            offsetWORK
          );
          dger(
            lastv,
            lastc,
            -tau,
            v,
            strideV,
            offsetV,
            WORK,
            strideWORK,
            offsetWORK,
            C,
            strideC1,
            strideC2,
            offsetC
          );
        }
      } else if (lastv > 0) {
        dgemv(
          "no-transpose",
          lastc,
          lastv,
          1,
          C,
          strideC1,
          strideC2,
          offsetC,
          v,
          strideV,
          offsetV,
          0,
          WORK,
          strideWORK,
          offsetWORK
        );
        dger(
          lastc,
          lastv,
          -tau,
          WORK,
          strideWORK,
          offsetWORK,
          v,
          strideV,
          offsetV,
          C,
          strideC1,
          strideC2,
          offsetC
        );
      }
    }
    module.exports = dlarf;
  }
});

// lib/lapack/base/dlapy2/lib/base.js
var require_base19 = __commonJS({
  "lib/lapack/base/dlapy2/lib/base.js"(exports, module) {
    "use strict";
    function dlapy2(x, y) {
      var xabs;
      var yabs;
      var w;
      var z;
      if (x !== x) {
        return x;
      }
      if (y !== y) {
        return y;
      }
      xabs = Math.abs(x);
      yabs = Math.abs(y);
      w = Math.max(xabs, yabs);
      z = Math.min(xabs, yabs);
      if (z === 0 || w > 17976931348623157e292) {
        return w;
      }
      return w * Math.sqrt(1 + z / w * (z / w));
    }
    module.exports = dlapy2;
  }
});

// lib/lapack/base/dlarfg/lib/base.js
var require_base20 = __commonJS({
  "lib/lapack/base/dlarfg/lib/base.js"(exports, module) {
    "use strict";
    var dnrm2 = require_base2();
    var dscal = require_base();
    var dlamch = require_base5();
    var dlapy2 = require_base19();
    function dlarfg(N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau) {
      var rsafmn;
      var safmin;
      var xnorm;
      var beta;
      var knt;
      var j;
      if (N <= 1) {
        tau[offsetTau] = 0;
        return;
      }
      xnorm = dnrm2(N - 1, x, strideX, offsetX);
      if (xnorm === 0) {
        tau[offsetTau] = 0;
      } else {
        beta = -(Math.sign(alpha[offsetAlpha]) || 1) * dlapy2(alpha[offsetAlpha], xnorm);
        safmin = dlamch("safe-minimum") / dlamch("epsilon");
        knt = 0;
        if (Math.abs(beta) < safmin) {
          rsafmn = 1 / safmin;
          do {
            knt += 1;
            dscal(N - 1, rsafmn, x, strideX, offsetX);
            beta *= rsafmn;
            alpha[offsetAlpha] *= rsafmn;
          } while (Math.abs(beta) < safmin && knt < 20);
          xnorm = dnrm2(N - 1, x, strideX, offsetX);
          beta = -(Math.sign(alpha[offsetAlpha]) || 1) * dlapy2(alpha[offsetAlpha], xnorm);
        }
        tau[offsetTau] = (beta - alpha[offsetAlpha]) / beta;
        dscal(N - 1, 1 / (alpha[offsetAlpha] - beta), x, strideX, offsetX);
        for (j = 0; j < knt; j++) {
          beta *= safmin;
        }
        alpha[offsetAlpha] = beta;
      }
    }
    module.exports = dlarfg;
  }
});

// lib/lapack/base/dgehd2/lib/base.js
var require_base21 = __commonJS({
  "lib/lapack/base/dgehd2/lib/base.js"(exports, module) {
    "use strict";
    var dlarf = require_base18();
    var dlarfg = require_base20();
    function dgehd2(N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var xStart;
      var oAlpha;
      var oTau;
      var aii;
      var i;
      for (i = ilo - 1; i < ihi - 1; i++) {
        oAlpha = offsetA + (i + 1) * strideA1 + i * strideA2;
        xStart = Math.min(i + 2, N - 1);
        oTau = offsetTAU + i * strideTAU;
        dlarfg(ihi - i - 1, A, oAlpha, A, strideA1, offsetA + xStart * strideA1 + i * strideA2, TAU, oTau);
        aii = A[oAlpha];
        A[oAlpha] = 1;
        dlarf("right", ihi, ihi - i - 1, A, strideA1, offsetA + (i + 1) * strideA1 + i * strideA2, TAU[oTau], A, strideA1, strideA2, offsetA + (i + 1) * strideA2, WORK, strideWORK, offsetWORK);
        dlarf("left", ihi - i - 1, N - i - 1, A, strideA1, offsetA + (i + 1) * strideA1 + i * strideA2, TAU[oTau], A, strideA1, strideA2, offsetA + (i + 1) * strideA1 + (i + 1) * strideA2, WORK, strideWORK, offsetWORK);
        A[oAlpha] = aii;
      }
      return 0;
    }
    module.exports = dgehd2;
  }
});

// lib/blas/base/dgemm/lib/base.js
var require_base22 = __commonJS({
  "lib/blas/base/dgemm/lib/base.js"(exports, module) {
    "use strict";
    function dgemm2(transa, transb, M, N, K, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC) {
      var nota;
      var notb;
      var temp;
      var sa1;
      var sa2;
      var sb1;
      var sb2;
      var sc1;
      var sc2;
      var ia;
      var ib;
      var ic;
      var i;
      var j;
      var l;
      nota = transa === "no-transpose";
      notb = transb === "no-transpose";
      if (M === 0 || N === 0 || (alpha === 0 || K === 0) && beta === 1) {
        return C;
      }
      sa1 = strideA1;
      sa2 = strideA2;
      sb1 = strideB1;
      sb2 = strideB2;
      sc1 = strideC1;
      sc2 = strideC2;
      if (alpha === 0) {
        if (beta === 0) {
          for (j = 0; j < N; j++) {
            ic = offsetC + j * sc2;
            for (i = 0; i < M; i++) {
              C[ic] = 0;
              ic += sc1;
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            ic = offsetC + j * sc2;
            for (i = 0; i < M; i++) {
              C[ic] *= beta;
              ic += sc1;
            }
          }
        }
        return C;
      }
      if (notb) {
        if (nota) {
          for (j = 0; j < N; j++) {
            if (beta === 0) {
              ic = offsetC + j * sc2;
              for (i = 0; i < M; i++) {
                C[ic] = 0;
                ic += sc1;
              }
            } else if (beta !== 1) {
              ic = offsetC + j * sc2;
              for (i = 0; i < M; i++) {
                C[ic] *= beta;
                ic += sc1;
              }
            }
            for (l = 0; l < K; l++) {
              temp = alpha * B[offsetB + l * sb1 + j * sb2];
              ia = offsetA + l * sa2;
              ic = offsetC + j * sc2;
              for (i = 0; i < M; i++) {
                C[ic] += temp * A[ia];
                ia += sa1;
                ic += sc1;
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              temp = 0;
              ia = offsetA + i * sa2;
              ib = offsetB + j * sb2;
              for (l = 0; l < K; l++) {
                temp += A[ia] * B[ib];
                ia += sa1;
                ib += sb1;
              }
              ic = offsetC + i * sc1 + j * sc2;
              if (beta === 0) {
                C[ic] = alpha * temp;
              } else {
                C[ic] = alpha * temp + beta * C[ic];
              }
            }
          }
        }
      } else if (nota) {
        for (j = 0; j < N; j++) {
          if (beta === 0) {
            ic = offsetC + j * sc2;
            for (i = 0; i < M; i++) {
              C[ic] = 0;
              ic += sc1;
            }
          } else if (beta !== 1) {
            ic = offsetC + j * sc2;
            for (i = 0; i < M; i++) {
              C[ic] *= beta;
              ic += sc1;
            }
          }
          for (l = 0; l < K; l++) {
            temp = alpha * B[offsetB + j * sb1 + l * sb2];
            ia = offsetA + l * sa2;
            ic = offsetC + j * sc2;
            for (i = 0; i < M; i++) {
              C[ic] += temp * A[ia];
              ia += sa1;
              ic += sc1;
            }
          }
        }
      } else {
        for (j = 0; j < N; j++) {
          for (i = 0; i < M; i++) {
            temp = 0;
            ia = offsetA + i * sa2;
            ib = offsetB + j * sb1;
            for (l = 0; l < K; l++) {
              temp += A[ia] * B[ib];
              ia += sa1;
              ib += sb2;
            }
            ic = offsetC + i * sc1 + j * sc2;
            if (beta === 0) {
              C[ic] = alpha * temp;
            } else {
              C[ic] = alpha * temp + beta * C[ic];
            }
          }
        }
      }
      return C;
    }
    module.exports = dgemm2;
  }
});

// lib/blas/base/dcopy/lib/base.js
var require_base23 = __commonJS({
  "lib/blas/base/dcopy/lib/base.js"(exports, module) {
    "use strict";
    var M = 7;
    function dcopy(N, x, strideX, offsetX, y, strideY, offsetY) {
      var ix;
      var iy;
      var m;
      var i;
      if (N <= 0) {
        return y;
      }
      ix = offsetX;
      iy = offsetY;
      if (strideX === 1 && strideY === 1) {
        m = N % M;
        if (m > 0) {
          for (i = 0; i < m; i++) {
            y[iy] = x[ix];
            ix += 1;
            iy += 1;
          }
        }
        if (N < M) {
          return y;
        }
        for (i = m; i < N; i += M) {
          y[iy] = x[ix];
          y[iy + 1] = x[ix + 1];
          y[iy + 2] = x[ix + 2];
          y[iy + 3] = x[ix + 3];
          y[iy + 4] = x[ix + 4];
          y[iy + 5] = x[ix + 5];
          y[iy + 6] = x[ix + 6];
          ix += M;
          iy += M;
        }
        return y;
      }
      for (i = 0; i < N; i++) {
        y[iy] = x[ix];
        ix += strideX;
        iy += strideY;
      }
      return y;
    }
    module.exports = dcopy;
  }
});

// lib/blas/base/dtrmm/lib/base.js
var require_base24 = __commonJS({
  "lib/blas/base/dtrmm/lib/base.js"(exports, module) {
    "use strict";
    function dtrmm(side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB) {
      var nounit;
      var lside;
      var upper;
      var temp;
      var sa1;
      var sa2;
      var sb1;
      var sb2;
      var ia;
      var ib;
      var i;
      var j;
      var k;
      if (M === 0 || N === 0) {
        return B;
      }
      lside = side === "left";
      upper = uplo === "upper";
      nounit = diag === "non-unit";
      sa1 = strideA1;
      sa2 = strideA2;
      sb1 = strideB1;
      sb2 = strideB2;
      if (alpha === 0) {
        for (j = 0; j < N; j++) {
          ib = offsetB + j * sb2;
          for (i = 0; i < M; i++) {
            B[ib] = 0;
            ib += sb1;
          }
        }
        return B;
      }
      if (lside) {
        if (transa === "no-transpose") {
          if (upper) {
            for (j = 0; j < N; j++) {
              for (k = 0; k < M; k++) {
                ib = offsetB + k * sb1 + j * sb2;
                if (B[ib] !== 0) {
                  temp = alpha * B[ib];
                  ia = offsetA + k * sa2;
                  for (i = 0; i < k; i++) {
                    B[offsetB + i * sb1 + j * sb2] += temp * A[ia];
                    ia += sa1;
                  }
                  if (nounit) {
                    B[ib] = temp * A[offsetA + k * sa1 + k * sa2];
                  } else {
                    B[ib] = temp;
                  }
                }
              }
            }
          } else {
            for (j = 0; j < N; j++) {
              for (k = M - 1; k >= 0; k--) {
                ib = offsetB + k * sb1 + j * sb2;
                if (B[ib] !== 0) {
                  temp = alpha * B[ib];
                  B[ib] = temp;
                  if (nounit) {
                    B[ib] = temp * A[offsetA + k * sa1 + k * sa2];
                  }
                  ia = offsetA + (k + 1) * sa1 + k * sa2;
                  for (i = k + 1; i < M; i++) {
                    B[offsetB + i * sb1 + j * sb2] += temp * A[ia];
                    ia += sa1;
                  }
                }
              }
            }
          }
        } else if (upper) {
          for (j = 0; j < N; j++) {
            for (i = M - 1; i >= 0; i--) {
              temp = B[offsetB + i * sb1 + j * sb2];
              if (nounit) {
                temp *= A[offsetA + i * sa1 + i * sa2];
              }
              ia = offsetA + i * sa2;
              for (k = 0; k < i; k++) {
                temp += A[ia] * B[offsetB + k * sb1 + j * sb2];
                ia += sa1;
              }
              B[offsetB + i * sb1 + j * sb2] = alpha * temp;
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              temp = B[offsetB + i * sb1 + j * sb2];
              if (nounit) {
                temp *= A[offsetA + i * sa1 + i * sa2];
              }
              for (k = i + 1; k < M; k++) {
                temp += A[offsetA + k * sa1 + i * sa2] * B[offsetB + k * sb1 + j * sb2];
              }
              B[offsetB + i * sb1 + j * sb2] = alpha * temp;
            }
          }
        }
      } else if (transa === "no-transpose") {
        if (upper) {
          for (j = N - 1; j >= 0; j--) {
            temp = alpha;
            if (nounit) {
              temp *= A[offsetA + j * sa1 + j * sa2];
            }
            ib = offsetB + j * sb2;
            for (i = 0; i < M; i++) {
              B[ib] *= temp;
              ib += sb1;
            }
            for (k = 0; k < j; k++) {
              if (A[offsetA + k * sa1 + j * sa2] !== 0) {
                temp = alpha * A[offsetA + k * sa1 + j * sa2];
                for (i = 0; i < M; i++) {
                  B[offsetB + i * sb1 + j * sb2] += temp * B[offsetB + i * sb1 + k * sb2];
                }
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            temp = alpha;
            if (nounit) {
              temp *= A[offsetA + j * sa1 + j * sa2];
            }
            ib = offsetB + j * sb2;
            for (i = 0; i < M; i++) {
              B[ib] *= temp;
              ib += sb1;
            }
            for (k = j + 1; k < N; k++) {
              if (A[offsetA + k * sa1 + j * sa2] !== 0) {
                temp = alpha * A[offsetA + k * sa1 + j * sa2];
                for (i = 0; i < M; i++) {
                  B[offsetB + i * sb1 + j * sb2] += temp * B[offsetB + i * sb1 + k * sb2];
                }
              }
            }
          }
        }
      } else if (upper) {
        for (k = 0; k < N; k++) {
          for (j = 0; j < k; j++) {
            if (A[offsetA + j * sa1 + k * sa2] !== 0) {
              temp = alpha * A[offsetA + j * sa1 + k * sa2];
              for (i = 0; i < M; i++) {
                B[offsetB + i * sb1 + j * sb2] += temp * B[offsetB + i * sb1 + k * sb2];
              }
            }
          }
          temp = alpha;
          if (nounit) {
            temp *= A[offsetA + k * sa1 + k * sa2];
          }
          if (temp !== 1) {
            ib = offsetB + k * sb2;
            for (i = 0; i < M; i++) {
              B[ib] *= temp;
              ib += sb1;
            }
          }
        }
      } else {
        for (k = N - 1; k >= 0; k--) {
          for (j = k + 1; j < N; j++) {
            if (A[offsetA + j * sa1 + k * sa2] !== 0) {
              temp = alpha * A[offsetA + j * sa1 + k * sa2];
              for (i = 0; i < M; i++) {
                B[offsetB + i * sb1 + j * sb2] += temp * B[offsetB + i * sb1 + k * sb2];
              }
            }
          }
          temp = alpha;
          if (nounit) {
            temp *= A[offsetA + k * sa1 + k * sa2];
          }
          if (temp !== 1) {
            ib = offsetB + k * sb2;
            for (i = 0; i < M; i++) {
              B[ib] *= temp;
              ib += sb1;
            }
          }
        }
      }
      return B;
    }
    module.exports = dtrmm;
  }
});

// lib/blas/base/dtrmv/lib/base.js
var require_base25 = __commonJS({
  "lib/blas/base/dtrmv/lib/base.js"(exports, module) {
    "use strict";
    function dtrmv(uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX) {
      var nounit;
      var temp;
      var sa1;
      var sa2;
      var ix;
      var jx;
      var ia;
      var i;
      var j;
      if (N <= 0) {
        return x;
      }
      nounit = diag === "non-unit";
      sa1 = strideA1;
      sa2 = strideA2;
      if (trans === "no-transpose") {
        if (uplo === "upper") {
          jx = offsetX;
          for (j = 0; j < N; j++) {
            if (x[jx] !== 0) {
              temp = x[jx];
              ix = offsetX;
              ia = offsetA + j * sa2;
              for (i = 0; i < j; i++) {
                x[ix] += temp * A[ia];
                ix += strideX;
                ia += sa1;
              }
              if (nounit) {
                x[jx] *= A[offsetA + j * sa1 + j * sa2];
              }
            }
            jx += strideX;
          }
        } else {
          jx = offsetX + (N - 1) * strideX;
          for (j = N - 1; j >= 0; j--) {
            if (x[jx] !== 0) {
              temp = x[jx];
              ix = offsetX + (N - 1) * strideX;
              ia = offsetA + (N - 1) * sa1 + j * sa2;
              for (i = N - 1; i > j; i--) {
                x[ix] += temp * A[ia];
                ix -= strideX;
                ia -= sa1;
              }
              if (nounit) {
                x[jx] *= A[offsetA + j * sa1 + j * sa2];
              }
            }
            jx -= strideX;
          }
        }
      } else if (uplo === "upper") {
        jx = offsetX + (N - 1) * strideX;
        for (j = N - 1; j >= 0; j--) {
          temp = x[jx];
          if (nounit) {
            temp *= A[offsetA + j * sa1 + j * sa2];
          }
          ix = offsetX + (j - 1) * strideX;
          ia = offsetA + (j - 1) * sa1 + j * sa2;
          for (i = j - 1; i >= 0; i--) {
            temp += A[ia] * x[ix];
            ix -= strideX;
            ia -= sa1;
          }
          x[jx] = temp;
          jx -= strideX;
        }
      } else {
        jx = offsetX;
        for (j = 0; j < N; j++) {
          temp = x[jx];
          if (nounit) {
            temp *= A[offsetA + j * sa1 + j * sa2];
          }
          ix = offsetX + (j + 1) * strideX;
          ia = offsetA + (j + 1) * sa1 + j * sa2;
          for (i = j + 1; i < N; i++) {
            temp += A[ia] * x[ix];
            ix += strideX;
            ia += sa1;
          }
          x[jx] = temp;
          jx += strideX;
        }
      }
      return x;
    }
    module.exports = dtrmv;
  }
});

// lib/lapack/base/dlahr2/lib/base.js
var require_base26 = __commonJS({
  "lib/lapack/base/dlahr2/lib/base.js"(exports, module) {
    "use strict";
    var daxpy = require_base13();
    var dcopy = require_base23();
    var dgemm2 = require_base22();
    var dgemv = require_base14();
    var dlacpy = require_base9();
    var dlarfg = require_base20();
    var dscal = require_base();
    var dtrmm = require_base24();
    var dtrmv = require_base25();
    function dlahr2(N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT, offsetT, ldT, Y, strideY, offsetY, ldY) {
      var ei;
      var i;
      if (N <= 1) {
        return;
      }
      ei = 0;
      for (i = 0; i < nb; i++) {
        if (i > 0) {
          dgemv("no-transpose", N - K, i, -1, Y, strideY, ldY, offsetY + K * strideY, A, strideA2, offsetA + (K + i - 1) * strideA1, 1, A, strideA1, offsetA + K * strideA1 + i * strideA2);
          dcopy(i, A, strideA1, offsetA + K * strideA1 + i * strideA2, T, strideT, offsetT + (nb - 1) * ldT);
          dtrmv("lower", "transpose", "unit", i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT, offsetT + (nb - 1) * ldT);
          dgemv("transpose", N - K - i, i, 1, A, strideA1, strideA2, offsetA + (K + i) * strideA1, A, strideA1, offsetA + (K + i) * strideA1 + i * strideA2, 1, T, strideT, offsetT + (nb - 1) * ldT);
          dtrmv("upper", "transpose", "non-unit", i, T, strideT, ldT, offsetT, T, strideT, offsetT + (nb - 1) * ldT);
          dgemv("no-transpose", N - K - i, i, -1, A, strideA1, strideA2, offsetA + (K + i) * strideA1, T, strideT, offsetT + (nb - 1) * ldT, 1, A, strideA1, offsetA + (K + i) * strideA1 + i * strideA2);
          dtrmv("lower", "no-transpose", "unit", i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT, offsetT + (nb - 1) * ldT);
          daxpy(i, -1, T, strideT, offsetT + (nb - 1) * ldT, A, strideA1, offsetA + K * strideA1 + i * strideA2);
          A[offsetA + (K + i - 1) * strideA1 + (i - 1) * strideA2] = ei;
        }
        dlarfg(N - K - i, A, offsetA + (K + i) * strideA1 + i * strideA2, A, strideA1, offsetA + Math.min(K + i + 1, N - 1) * strideA1 + i * strideA2, tau, offsetTAU + i * strideTAU);
        ei = A[offsetA + (K + i) * strideA1 + i * strideA2];
        A[offsetA + (K + i) * strideA1 + i * strideA2] = 1;
        dgemv("no-transpose", N - K, N - K - i, 1, A, strideA1, strideA2, offsetA + K * strideA1 + (i + 1) * strideA2, A, strideA1, offsetA + (K + i) * strideA1 + i * strideA2, 0, Y, strideY, offsetY + K * strideY + i * ldY);
        dgemv("transpose", N - K - i, i, 1, A, strideA1, strideA2, offsetA + (K + i) * strideA1, A, strideA1, offsetA + (K + i) * strideA1 + i * strideA2, 0, T, strideT, offsetT + i * ldT);
        dgemv("no-transpose", N - K, i, -1, Y, strideY, ldY, offsetY + K * strideY, T, strideT, offsetT + i * ldT, 1, Y, strideY, offsetY + K * strideY + i * ldY);
        dscal(N - K, tau[offsetTAU + i * strideTAU], Y, strideY, offsetY + K * strideY + i * ldY);
        dscal(i, -tau[offsetTAU + i * strideTAU], T, strideT, offsetT + i * ldT);
        dtrmv("upper", "no-transpose", "non-unit", i, T, strideT, ldT, offsetT, T, strideT, offsetT + i * ldT);
        T[offsetT + i * strideT + i * ldT] = tau[offsetTAU + i * strideTAU];
      }
      A[offsetA + (K + nb - 1) * strideA1 + (nb - 1) * strideA2] = ei;
      dlacpy("all", K, nb, A, strideA1, strideA2, offsetA + 1 * strideA2, Y, strideY, ldY, offsetY);
      dtrmm("right", "lower", "no-transpose", "unit", K, nb, 1, A, strideA1, strideA2, offsetA + K * strideA1, Y, strideY, ldY, offsetY);
      if (N > K + nb) {
        dgemm2("no-transpose", "no-transpose", K, nb, N - K - nb, 1, A, strideA1, strideA2, offsetA + (1 + nb) * strideA2, A, strideA1, strideA2, offsetA + (K + nb) * strideA1, 1, Y, strideY, ldY, offsetY);
      }
      dtrmm("right", "upper", "no-transpose", "non-unit", K, nb, 1, T, strideT, ldT, offsetT, Y, strideY, ldY, offsetY);
    }
    module.exports = dlahr2;
  }
});

// lib/lapack/base/dlarfb/lib/base.js
var require_base27 = __commonJS({
  "lib/lapack/base/dlarfb/lib/base.js"(exports, module) {
    "use strict";
    var dcopy = require_base23();
    var dgemm2 = require_base22();
    var dtrmm = require_base24();
    function dlarfb(side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK) {
      var transt;
      var i;
      var j;
      if (M <= 0 || N <= 0) {
        return;
      }
      if (trans === "no-transpose") {
        transt = "transpose";
      } else {
        transt = "no-transpose";
      }
      if (storev === "columnwise") {
        if (direct === "forward") {
          if (side === "left") {
            for (j = 0; j < K; j++) {
              dcopy(N, C, strideC2, offsetC + j * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            dtrmm("right", "lower", "no-transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              dgemm2(
                "transpose",
                "no-transpose",
                N,
                K,
                M - K,
                1,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                1,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            dtrmm("right", "upper", transt, "non-unit", N, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              dgemm2(
                "no-transpose",
                "transpose",
                M - K,
                N,
                K,
                -1,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                1,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1
              );
            }
            dtrmm("right", "lower", "transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < N; i++) {
                C[offsetC + j * strideC1 + i * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
              }
            }
          } else if (side === "right") {
            for (j = 0; j < K; j++) {
              dcopy(M, C, strideC1, offsetC + j * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            dtrmm("right", "lower", "no-transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                K,
                N - K,
                1,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                1,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            dtrmm("right", "upper", trans, "non-unit", M, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              dgemm2(
                "no-transpose",
                "transpose",
                M,
                N - K,
                K,
                -1,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                1,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2
              );
            }
            dtrmm("right", "lower", "transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < M; i++) {
                C[offsetC + i * strideC1 + j * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
              }
            }
          }
        } else if (side === "left") {
          for (j = 0; j < K; j++) {
            dcopy(N, C, strideC2, offsetC + (M - K + j) * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
          }
          dtrmm("right", "upper", "no-transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV + (M - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (M > K) {
            dgemm2(
              "transpose",
              "no-transpose",
              N,
              K,
              M - K,
              1,
              C,
              strideC1,
              strideC2,
              offsetC,
              V,
              strideV1,
              strideV2,
              offsetV,
              1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK
            );
          }
          dtrmm("right", "lower", transt, "non-unit", N, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (M > K) {
            dgemm2(
              "no-transpose",
              "transpose",
              M - K,
              N,
              K,
              -1,
              V,
              strideV1,
              strideV2,
              offsetV,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK,
              1,
              C,
              strideC1,
              strideC2,
              offsetC
            );
          }
          dtrmm("right", "upper", "transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV + (M - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
          for (j = 0; j < K; j++) {
            for (i = 0; i < N; i++) {
              C[offsetC + (M - K + j) * strideC1 + i * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
            }
          }
        } else if (side === "right") {
          for (j = 0; j < K; j++) {
            dcopy(M, C, strideC1, offsetC + (N - K + j) * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
          }
          dtrmm("right", "upper", "no-transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV + (N - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (N > K) {
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              K,
              N - K,
              1,
              C,
              strideC1,
              strideC2,
              offsetC,
              V,
              strideV1,
              strideV2,
              offsetV,
              1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK
            );
          }
          dtrmm("right", "lower", trans, "non-unit", M, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (N > K) {
            dgemm2(
              "no-transpose",
              "transpose",
              M,
              N - K,
              K,
              -1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK,
              V,
              strideV1,
              strideV2,
              offsetV,
              1,
              C,
              strideC1,
              strideC2,
              offsetC
            );
          }
          dtrmm("right", "upper", "transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV + (N - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
          for (j = 0; j < K; j++) {
            for (i = 0; i < M; i++) {
              C[offsetC + i * strideC1 + (N - K + j) * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
            }
          }
        }
      } else if (direct === "forward") {
        if (side === "left") {
          for (j = 0; j < K; j++) {
            dcopy(N, C, strideC2, offsetC + j * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
          }
          dtrmm("right", "upper", "transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (M > K) {
            dgemm2(
              "transpose",
              "transpose",
              N,
              K,
              M - K,
              1,
              C,
              strideC1,
              strideC2,
              offsetC + K * strideC1,
              V,
              strideV1,
              strideV2,
              offsetV + K * strideV2,
              1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK
            );
          }
          dtrmm("right", "upper", transt, "non-unit", N, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (M > K) {
            dgemm2(
              "transpose",
              "transpose",
              M - K,
              N,
              K,
              -1,
              V,
              strideV1,
              strideV2,
              offsetV + K * strideV2,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK,
              1,
              C,
              strideC1,
              strideC2,
              offsetC + K * strideC1
            );
          }
          dtrmm("right", "upper", "no-transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
          for (j = 0; j < K; j++) {
            for (i = 0; i < N; i++) {
              C[offsetC + j * strideC1 + i * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
            }
          }
        } else if (side === "right") {
          for (j = 0; j < K; j++) {
            dcopy(M, C, strideC1, offsetC + j * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
          }
          dtrmm("right", "upper", "transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (N > K) {
            dgemm2(
              "no-transpose",
              "transpose",
              M,
              K,
              N - K,
              1,
              C,
              strideC1,
              strideC2,
              offsetC + K * strideC2,
              V,
              strideV1,
              strideV2,
              offsetV + K * strideV2,
              1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK
            );
          }
          dtrmm("right", "upper", trans, "non-unit", M, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
          if (N > K) {
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N - K,
              K,
              -1,
              WORK,
              strideWORK1,
              strideWORK2,
              offsetWORK,
              V,
              strideV1,
              strideV2,
              offsetV + K * strideV2,
              1,
              C,
              strideC1,
              strideC2,
              offsetC + K * strideC2
            );
          }
          dtrmm("right", "upper", "no-transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
          for (j = 0; j < K; j++) {
            for (i = 0; i < M; i++) {
              C[offsetC + i * strideC1 + j * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
            }
          }
        }
      } else if (side === "left") {
        for (j = 0; j < K; j++) {
          dcopy(N, C, strideC2, offsetC + (M - K + j) * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
        }
        dtrmm("right", "lower", "transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV + (M - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
        if (M > K) {
          dgemm2(
            "transpose",
            "transpose",
            N,
            K,
            M - K,
            1,
            C,
            strideC1,
            strideC2,
            offsetC,
            V,
            strideV1,
            strideV2,
            offsetV,
            1,
            WORK,
            strideWORK1,
            strideWORK2,
            offsetWORK
          );
        }
        dtrmm("right", "lower", transt, "non-unit", N, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
        if (M > K) {
          dgemm2(
            "transpose",
            "transpose",
            M - K,
            N,
            K,
            -1,
            V,
            strideV1,
            strideV2,
            offsetV,
            WORK,
            strideWORK1,
            strideWORK2,
            offsetWORK,
            1,
            C,
            strideC1,
            strideC2,
            offsetC
          );
        }
        dtrmm("right", "lower", "no-transpose", "unit", N, K, 1, V, strideV1, strideV2, offsetV + (M - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
        for (j = 0; j < K; j++) {
          for (i = 0; i < N; i++) {
            C[offsetC + (M - K + j) * strideC1 + i * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
          }
        }
      } else if (side === "right") {
        for (j = 0; j < K; j++) {
          dcopy(M, C, strideC1, offsetC + (N - K + j) * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
        }
        dtrmm("right", "lower", "transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV + (N - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
        if (N > K) {
          dgemm2(
            "no-transpose",
            "transpose",
            M,
            K,
            N - K,
            1,
            C,
            strideC1,
            strideC2,
            offsetC,
            V,
            strideV1,
            strideV2,
            offsetV,
            1,
            WORK,
            strideWORK1,
            strideWORK2,
            offsetWORK
          );
        }
        dtrmm("right", "lower", trans, "non-unit", M, K, 1, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
        if (N > K) {
          dgemm2(
            "no-transpose",
            "no-transpose",
            M,
            N - K,
            K,
            -1,
            WORK,
            strideWORK1,
            strideWORK2,
            offsetWORK,
            V,
            strideV1,
            strideV2,
            offsetV,
            1,
            C,
            strideC1,
            strideC2,
            offsetC
          );
        }
        dtrmm("right", "lower", "no-transpose", "unit", M, K, 1, V, strideV1, strideV2, offsetV + (N - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
        for (j = 0; j < K; j++) {
          for (i = 0; i < M; i++) {
            C[offsetC + i * strideC1 + (N - K + j) * strideC2] -= WORK[offsetWORK + i * strideWORK1 + j * strideWORK2];
          }
        }
      }
    }
    module.exports = dlarfb;
  }
});

// lib/lapack/base/dgehrd/lib/base.js
var require_base28 = __commonJS({
  "lib/lapack/base/dgehrd/lib/base.js"(exports, module) {
    "use strict";
    var daxpy = require_base13();
    var dgehd2 = require_base21();
    var dgemm2 = require_base22();
    var dlahr2 = require_base26();
    var dlarfb = require_base27();
    var dtrmm = require_base24();
    var NBMAX = 64;
    var LDT = NBMAX + 1;
    function dgehrd(N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var LDWORK;
      var IWT;
      var sa1;
      var sa2;
      var oA;
      var oE;
      var NB;
      var NX;
      var NH;
      var IB;
      var ei;
      var i;
      var j;
      NB = 32;
      sa1 = strideA1;
      sa2 = strideA2;
      oA = offsetA;
      for (i = 0; i < ilo - 1; i++) {
        TAU[offsetTAU + i * strideTAU] = 0;
      }
      for (i = Math.max(0, ihi - 1); i < N - 1; i++) {
        TAU[offsetTAU + i * strideTAU] = 0;
      }
      NH = ihi - ilo + 1;
      if (NH <= 1) {
        return 0;
      }
      NX = Math.max(NB, NB);
      LDWORK = N;
      if (NB < 2 || NB >= NH) {
        i = ilo;
      } else {
        IWT = N * NB;
        for (i = ilo; i <= ihi - 1 - NX; i += NB) {
          IB = Math.min(NB, ihi - i);
          dlahr2(ihi, i, IB, A, strideA1, strideA2, offsetA + (i - 1) * strideA2, TAU, strideTAU, offsetTAU + (i - 1) * strideTAU, WORK, 1, offsetWORK + IWT, LDT, WORK, 1, offsetWORK, LDWORK);
          oE = oA + (i + IB - 1) * sa1 + (i + IB - 2) * sa2;
          ei = A[oE];
          A[oE] = 1;
          dgemm2("no-transpose", "transpose", ihi, ihi - i - IB + 1, IB, -1, WORK, 1, LDWORK, offsetWORK, A, strideA1, strideA2, offsetA + (i + IB - 1) * strideA1 + (i - 1) * strideA2, 1, A, strideA1, strideA2, offsetA + (i + IB - 1) * strideA2);
          A[oE] = ei;
          dtrmm("right", "lower", "transpose", "unit", i, IB - 1, 1, A, strideA1, strideA2, offsetA + i * strideA1 + (i - 1) * strideA2, WORK, 1, LDWORK, offsetWORK);
          for (j = 0; j < IB - 1; j++) {
            daxpy(i, -1, WORK, 1, offsetWORK + LDWORK * j, A, strideA1, offsetA + (i + j) * strideA2);
          }
          dlarfb("left", "transpose", "forward", "columnwise", ihi - i, N - i - IB + 1, IB, A, strideA1, strideA2, offsetA + i * strideA1 + (i - 1) * strideA2, WORK, 1, LDT, offsetWORK + IWT, A, strideA1, strideA2, offsetA + i * strideA1 + (i + IB - 1) * strideA2, WORK, 1, LDWORK, offsetWORK);
        }
      }
      dgehd2(N, i, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
      return 0;
    }
    module.exports = dgehrd;
  }
});

// node_modules/@stdlib/assert/has-symbol-support/lib/main.js
var require_main = __commonJS({
  "node_modules/@stdlib/assert/has-symbol-support/lib/main.js"(exports, module) {
    "use strict";
    function hasSymbolSupport() {
      return typeof Symbol === "function" && typeof /* @__PURE__ */ Symbol("foo") === "symbol";
    }
    module.exports = hasSymbolSupport;
  }
});

// node_modules/@stdlib/assert/has-symbol-support/lib/index.js
var require_lib = __commonJS({
  "node_modules/@stdlib/assert/has-symbol-support/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/has-tostringtag-support/lib/main.js
var require_main2 = __commonJS({
  "node_modules/@stdlib/assert/has-tostringtag-support/lib/main.js"(exports, module) {
    "use strict";
    var hasSymbols = require_lib();
    var FLG = hasSymbols();
    function hasToStringTagSupport() {
      return FLG && typeof Symbol.toStringTag === "symbol";
    }
    module.exports = hasToStringTagSupport;
  }
});

// node_modules/@stdlib/assert/has-tostringtag-support/lib/index.js
var require_lib2 = __commonJS({
  "node_modules/@stdlib/assert/has-tostringtag-support/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main2();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/native-class/lib/tostring.js
var require_tostring = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/tostring.js"(exports, module) {
    "use strict";
    var toStr = Object.prototype.toString;
    module.exports = toStr;
  }
});

// node_modules/@stdlib/utils/native-class/lib/main.js
var require_main3 = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/main.js"(exports, module) {
    "use strict";
    var toStr = require_tostring();
    function nativeClass(v) {
      return toStr.call(v);
    }
    module.exports = nativeClass;
  }
});

// node_modules/@stdlib/assert/has-own-property/lib/main.js
var require_main4 = __commonJS({
  "node_modules/@stdlib/assert/has-own-property/lib/main.js"(exports, module) {
    "use strict";
    var has = Object.prototype.hasOwnProperty;
    function hasOwnProp(value, property) {
      if (value === void 0 || value === null) {
        return false;
      }
      return has.call(value, property);
    }
    module.exports = hasOwnProp;
  }
});

// node_modules/@stdlib/assert/has-own-property/lib/index.js
var require_lib3 = __commonJS({
  "node_modules/@stdlib/assert/has-own-property/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main4();
    module.exports = main;
  }
});

// node_modules/@stdlib/symbol/ctor/lib/main.js
var require_main5 = __commonJS({
  "node_modules/@stdlib/symbol/ctor/lib/main.js"(exports, module) {
    "use strict";
    var Sym = typeof Symbol === "function" ? Symbol : void 0;
    module.exports = Sym;
  }
});

// node_modules/@stdlib/symbol/ctor/lib/index.js
var require_lib4 = __commonJS({
  "node_modules/@stdlib/symbol/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main5();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/native-class/lib/tostringtag.js
var require_tostringtag = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/tostringtag.js"(exports, module) {
    "use strict";
    var Symbol2 = require_lib4();
    var toStrTag = typeof Symbol2 === "function" ? Symbol2.toStringTag : "";
    module.exports = toStrTag;
  }
});

// node_modules/@stdlib/utils/native-class/lib/polyfill.js
var require_polyfill = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/polyfill.js"(exports, module) {
    "use strict";
    var hasOwnProp = require_lib3();
    var toStringTag = require_tostringtag();
    var toStr = require_tostring();
    function nativeClass(v) {
      var isOwn;
      var tag;
      var out;
      if (v === null || v === void 0) {
        return toStr.call(v);
      }
      tag = v[toStringTag];
      isOwn = hasOwnProp(v, toStringTag);
      try {
        v[toStringTag] = void 0;
      } catch (err) {
        return toStr.call(v);
      }
      out = toStr.call(v);
      if (isOwn) {
        v[toStringTag] = tag;
      } else {
        delete v[toStringTag];
      }
      return out;
    }
    module.exports = nativeClass;
  }
});

// node_modules/@stdlib/utils/native-class/lib/index.js
var require_lib5 = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/index.js"(exports, module) {
    "use strict";
    var hasToStringTag = require_lib2();
    var builtin = require_main3();
    var polyfill = require_polyfill();
    var main;
    if (hasToStringTag()) {
      main = polyfill;
    } else {
      main = builtin;
    }
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-float64array/lib/main.js
var require_main6 = __commonJS({
  "node_modules/@stdlib/assert/is-float64array/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib5();
    var hasFloat64Array = typeof Float64Array === "function";
    function isFloat64Array(value) {
      return hasFloat64Array && value instanceof Float64Array || // eslint-disable-line stdlib/require-globals
      nativeClass(value) === "[object Float64Array]";
    }
    module.exports = isFloat64Array;
  }
});

// node_modules/@stdlib/assert/is-float64array/lib/index.js
var require_lib6 = __commonJS({
  "node_modules/@stdlib/assert/is-float64array/lib/index.js"(exports, module) {
    "use strict";
    var isFloat64Array = require_main6();
    module.exports = isFloat64Array;
  }
});

// node_modules/@stdlib/assert/has-float64array-support/lib/float64array.js
var require_float64array = __commonJS({
  "node_modules/@stdlib/assert/has-float64array-support/lib/float64array.js"(exports, module) {
    "use strict";
    var main = typeof Float64Array === "function" ? Float64Array : null;
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/has-float64array-support/lib/main.js
var require_main7 = __commonJS({
  "node_modules/@stdlib/assert/has-float64array-support/lib/main.js"(exports, module) {
    "use strict";
    var isFloat64Array = require_lib6();
    var GlobalFloat64Array = require_float64array();
    function hasFloat64ArraySupport() {
      var bool;
      var arr;
      if (typeof GlobalFloat64Array !== "function") {
        return false;
      }
      try {
        arr = new GlobalFloat64Array([1, 3.14, -3.14, NaN]);
        bool = isFloat64Array(arr) && arr[0] === 1 && arr[1] === 3.14 && arr[2] === -3.14 && arr[3] !== arr[3];
      } catch (err) {
        bool = false;
      }
      return bool;
    }
    module.exports = hasFloat64ArraySupport;
  }
});

// node_modules/@stdlib/assert/has-float64array-support/lib/index.js
var require_lib7 = __commonJS({
  "node_modules/@stdlib/assert/has-float64array-support/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat64ArraySupport = require_main7();
    module.exports = hasFloat64ArraySupport;
  }
});

// node_modules/@stdlib/array/float64/lib/main.js
var require_main8 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/main.js"(exports, module) {
    "use strict";
    var ctor = typeof Float64Array === "function" ? Float64Array : void 0;
    module.exports = ctor;
  }
});

// node_modules/@stdlib/array/float64/lib/polyfill.js
var require_polyfill2 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/polyfill.js"(exports, module) {
    "use strict";
    function polyfill() {
      throw new Error("not implemented");
    }
    module.exports = polyfill;
  }
});

// node_modules/@stdlib/array/float64/lib/index.js
var require_lib8 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat64ArraySupport = require_lib7();
    var builtin = require_main8();
    var polyfill = require_polyfill2();
    var ctor;
    if (hasFloat64ArraySupport()) {
      ctor = builtin;
    } else {
      ctor = polyfill;
    }
    module.exports = ctor;
  }
});

// lib/lapack/base/dorg2r/lib/base.js
var require_base29 = __commonJS({
  "lib/lapack/base/dorg2r/lib/base.js"(exports, module) {
    "use strict";
    var dlarf = require_base18();
    var dscal = require_base();
    function dorg2r(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var i;
      var j;
      var l;
      if (N <= 0) {
        return 0;
      }
      for (j = K; j < N; j++) {
        for (l = 0; l < M; l++) {
          A[offsetA + l * strideA1 + j * strideA2] = 0;
        }
        A[offsetA + j * strideA1 + j * strideA2] = 1;
      }
      for (i = K - 1; i >= 0; i--) {
        if (i < N - 1) {
          A[offsetA + i * strideA1 + i * strideA2] = 1;
          dlarf(
            "left",
            M - i,
            N - i - 1,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2,
            TAU[offsetTAU + i * strideTAU],
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + (i + 1) * strideA2,
            WORK,
            strideWORK,
            offsetWORK
          );
        }
        if (i < M - 1) {
          dscal(
            M - i - 1,
            -TAU[offsetTAU + i * strideTAU],
            A,
            strideA1,
            offsetA + (i + 1) * strideA1 + i * strideA2
          );
        }
        A[offsetA + i * strideA1 + i * strideA2] = 1 - TAU[offsetTAU + i * strideTAU];
        for (l = 0; l < i; l++) {
          A[offsetA + l * strideA1 + i * strideA2] = 0;
        }
      }
      return 0;
    }
    module.exports = dorg2r;
  }
});

// lib/lapack/base/dlarft/lib/base.js
var require_base30 = __commonJS({
  "lib/lapack/base/dlarft/lib/base.js"(exports, module) {
    "use strict";
    var dgemv = require_base14();
    var dtrmv = require_base25();
    function dlarft(direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT) {
      var prevlastv;
      var lastv;
      var jj;
      var i;
      var j;
      if (N === 0) {
        return;
      }
      if (direct === "forward") {
        prevlastv = N;
        for (i = 0; i < K; i++) {
          prevlastv = Math.max(prevlastv, i);
          if (TAU[offsetTAU + i * strideTAU] === 0) {
            for (j = 0; j <= i; j++) {
              T[offsetT + j * strideT1 + i * strideT2] = 0;
            }
          } else {
            if (storev === "columnwise") {
              lastv = N;
              for (jj = N - 1; jj > i; jj--) {
                if (V[offsetV + jj * strideV1 + i * strideV2] !== 0) {
                  break;
                }
                lastv = jj;
              }
              for (j = 0; j < i; j++) {
                T[offsetT + j * strideT1 + i * strideT2] = -(TAU[offsetTAU + i * strideTAU] * V[offsetV + i * strideV1 + j * strideV2]);
              }
              jj = Math.min(lastv, prevlastv);
              if (jj - i - 1 > 0) {
                dgemv(
                  "transpose",
                  jj - i - 1,
                  i,
                  -TAU[offsetTAU + i * strideTAU],
                  V,
                  strideV1,
                  strideV2,
                  offsetV + (i + 1) * strideV1,
                  V,
                  strideV1,
                  offsetV + (i + 1) * strideV1 + i * strideV2,
                  1,
                  T,
                  strideT1,
                  offsetT + i * strideT2
                );
              }
            } else {
              lastv = N;
              for (jj = N - 1; jj > i; jj--) {
                if (V[offsetV + i * strideV1 + jj * strideV2] !== 0) {
                  break;
                }
                lastv = jj;
              }
              for (j = 0; j < i; j++) {
                T[offsetT + j * strideT1 + i * strideT2] = -(TAU[offsetTAU + i * strideTAU] * V[offsetV + j * strideV1 + i * strideV2]);
              }
              jj = Math.min(lastv, prevlastv);
              if (jj - i - 1 > 0) {
                dgemv(
                  "no-transpose",
                  i,
                  jj - i - 1,
                  -TAU[offsetTAU + i * strideTAU],
                  V,
                  strideV1,
                  strideV2,
                  offsetV + (i + 1) * strideV2,
                  V,
                  strideV2,
                  offsetV + i * strideV1 + (i + 1) * strideV2,
                  1,
                  T,
                  strideT1,
                  offsetT + i * strideT2
                );
              }
            }
            if (i > 0) {
              dtrmv(
                "upper",
                "no-transpose",
                "non-unit",
                i,
                T,
                strideT1,
                strideT2,
                offsetT,
                T,
                strideT1,
                offsetT + i * strideT2
              );
            }
            T[offsetT + i * strideT1 + i * strideT2] = TAU[offsetTAU + i * strideTAU];
            if (i > 0) {
              prevlastv = Math.max(prevlastv, lastv);
            } else {
              prevlastv = lastv;
            }
          }
        }
      } else {
        prevlastv = 0;
        for (i = K - 1; i >= 0; i--) {
          if (TAU[offsetTAU + i * strideTAU] === 0) {
            for (j = i; j < K; j++) {
              T[offsetT + j * strideT1 + i * strideT2] = 0;
            }
          } else {
            if (i < K - 1) {
              if (storev === "columnwise") {
                lastv = 0;
                for (jj = 0; jj < i; jj++) {
                  if (V[offsetV + jj * strideV1 + i * strideV2] !== 0) {
                    break;
                  }
                  lastv = jj + 1;
                }
                for (j = i + 1; j < K; j++) {
                  T[offsetT + j * strideT1 + i * strideT2] = -(TAU[offsetTAU + i * strideTAU] * V[offsetV + (N - K + i) * strideV1 + j * strideV2]);
                }
                jj = Math.max(lastv, prevlastv);
                if (N - K + i - jj > 0) {
                  dgemv(
                    "transpose",
                    N - K + i - jj,
                    K - i - 1,
                    -TAU[offsetTAU + i * strideTAU],
                    V,
                    strideV1,
                    strideV2,
                    offsetV + jj * strideV1 + (i + 1) * strideV2,
                    V,
                    strideV1,
                    offsetV + jj * strideV1 + i * strideV2,
                    1,
                    T,
                    strideT1,
                    offsetT + (i + 1) * strideT1 + i * strideT2
                  );
                }
              } else {
                lastv = 0;
                for (jj = 0; jj < i; jj++) {
                  if (V[offsetV + i * strideV1 + jj * strideV2] !== 0) {
                    break;
                  }
                  lastv = jj + 1;
                }
                for (j = i + 1; j < K; j++) {
                  T[offsetT + j * strideT1 + i * strideT2] = -(TAU[offsetTAU + i * strideTAU] * V[offsetV + j * strideV1 + (N - K + i) * strideV2]);
                }
                jj = Math.max(lastv, prevlastv);
                if (N - K + i - jj > 0) {
                  dgemv(
                    "no-transpose",
                    K - i - 1,
                    N - K + i - jj,
                    -TAU[offsetTAU + i * strideTAU],
                    V,
                    strideV1,
                    strideV2,
                    offsetV + (i + 1) * strideV1 + jj * strideV2,
                    V,
                    strideV2,
                    offsetV + i * strideV1 + jj * strideV2,
                    1,
                    T,
                    strideT1,
                    offsetT + (i + 1) * strideT1 + i * strideT2
                  );
                }
              }
              dtrmv(
                "lower",
                "no-transpose",
                "non-unit",
                K - i - 1,
                T,
                strideT1,
                strideT2,
                offsetT + (i + 1) * strideT1 + (i + 1) * strideT2,
                T,
                strideT1,
                offsetT + (i + 1) * strideT1 + i * strideT2
              );
              if (i > 0) {
                prevlastv = Math.min(prevlastv, lastv);
              } else {
                prevlastv = lastv;
              }
            }
            T[offsetT + i * strideT1 + i * strideT2] = TAU[offsetTAU + i * strideTAU];
          }
        }
      }
    }
    module.exports = dlarft;
  }
});

// lib/lapack/base/dorgqr/lib/base.js
var require_base31 = __commonJS({
  "lib/lapack/base/dorgqr/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dorg2r = require_base29();
    var dlarft = require_base30();
    var dlarfb = require_base27();
    var NB = 32;
    function dorgqr(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var ldwork;
      var work;
      var nb;
      var nx;
      var kk;
      var ki;
      var ib;
      var i;
      var j;
      var l;
      if (N <= 0) {
        return 0;
      }
      nb = NB;
      nx = 0;
      ldwork = N;
      if (nb >= 2 && nb < K) {
        nx = 0;
        if (nx < K) {
          ki = Math.floor((K - nx - 1) / nb) * nb;
          kk = Math.min(K, ki + nb);
          for (j = kk; j < N; j++) {
            for (i = 0; i < kk; i++) {
              A[offsetA + i * strideA1 + j * strideA2] = 0;
            }
          }
        }
      } else {
        kk = 0;
      }
      if (kk < N) {
        dorg2r(
          M - kk,
          N - kk,
          K - kk,
          A,
          strideA1,
          strideA2,
          offsetA + kk * strideA1 + kk * strideA2,
          TAU,
          strideTAU,
          offsetTAU + kk * strideTAU,
          WORK,
          strideWORK,
          offsetWORK
        );
      }
      if (kk > 0) {
        work = new Float64Array2(ldwork * nb);
        for (i = ki; i >= 0; i -= nb) {
          ib = Math.min(nb, K - i);
          if (i + ib < N) {
            dlarft(
              "forward",
              "columnwise",
              M - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAU,
              strideTAU,
              offsetTAU + i * strideTAU,
              work,
              1,
              ldwork,
              0
            );
            dlarfb(
              "left",
              "no-transpose",
              "forward",
              "columnwise",
              M - i,
              N - i - ib,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              work,
              1,
              ldwork,
              0,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + ib) * strideA2,
              work,
              1,
              ldwork,
              ib
            );
          }
          dorg2r(
            M - i,
            ib,
            ib,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + i * strideA2,
            TAU,
            strideTAU,
            offsetTAU + i * strideTAU,
            work,
            1,
            0
          );
          for (j = i; j < i + ib; j++) {
            for (l = 0; l < i; l++) {
              A[offsetA + l * strideA1 + j * strideA2] = 0;
            }
          }
        }
      }
      return 0;
    }
    module.exports = dorgqr;
  }
});

// lib/lapack/base/dorghr/lib/base.js
var require_base32 = __commonJS({
  "lib/lapack/base/dorghr/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dorgqr = require_base31();
    function dorghr(N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork) {
      var work;
      var nh;
      var sa1;
      var sa2;
      var oA;
      var i;
      var j;
      if (N === 0) {
        return 0;
      }
      sa1 = strideA1;
      sa2 = strideA2;
      oA = offsetA;
      nh = ihi - ilo;
      for (j = ihi - 1; j >= ilo; j--) {
        for (i = 0; i < j; i++) {
          A[oA + i * sa1 + j * sa2] = 0;
        }
        for (i = j + 1; i < ihi; i++) {
          A[oA + i * sa1 + j * sa2] = A[oA + i * sa1 + (j - 1) * sa2];
        }
        for (i = ihi; i < N; i++) {
          A[oA + i * sa1 + j * sa2] = 0;
        }
      }
      for (j = 0; j < ilo; j++) {
        for (i = 0; i < N; i++) {
          A[oA + i * sa1 + j * sa2] = 0;
        }
        A[oA + j * sa1 + j * sa2] = 1;
      }
      for (j = ihi; j < N; j++) {
        for (i = 0; i < N; i++) {
          A[oA + i * sa1 + j * sa2] = 0;
        }
        A[oA + j * sa1 + j * sa2] = 1;
      }
      if (nh > 0) {
        work = new Float64Array2(Math.max(1, nh) * 32);
        dorgqr(
          nh,
          nh,
          nh,
          A,
          sa1,
          sa2,
          oA + ilo * sa1 + ilo * sa2,
          TAU,
          strideTAU,
          offsetTAU + (ilo - 1) * strideTAU,
          work,
          1,
          0
        );
      }
      return 0;
    }
    module.exports = dorghr;
  }
});

// lib/lapack/base/dlanv2/lib/base.js
var require_base33 = __commonJS({
  "lib/lapack/base/dlanv2/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlapy2 = require_base19();
    var ZERO = 0;
    var HALF = 0.5;
    var ONE = 1;
    var TWO = 2;
    var MULTPL = 4;
    var EPS = dlamch("precision");
    var SAFMIN = dlamch("safe-minimum");
    var BASE = dlamch("base");
    var SAFMN2 = Math.pow(BASE, Math.floor(Math.log(SAFMIN / EPS) / Math.log(BASE) / TWO));
    var SAFMX2 = ONE / SAFMN2;
    function sign(a, b) {
      var mag = Math.abs(a);
      if (b > 0 || b === 0 && !Object.is(b, -0)) {
        return mag;
      }
      return -mag;
    }
    function dlanv2(A, B, C, D) {
      var bcmax;
      var bcmis;
      var scale;
      var sigma;
      var count;
      var temp;
      var tau;
      var rt1r;
      var rt1i;
      var rt2r;
      var rt2i;
      var cs;
      var sn;
      var cs1;
      var sn1;
      var sab;
      var sac;
      var aa;
      var bb;
      var cc;
      var dd;
      var p;
      var z;
      if (C === ZERO) {
        cs = ONE;
        sn = ZERO;
      } else if (B === ZERO) {
        cs = ZERO;
        sn = ONE;
        temp = D;
        D = A;
        A = temp;
        B = -C;
        C = ZERO;
      } else if (A - D === ZERO && sign(ONE, B) !== sign(ONE, C)) {
        cs = ONE;
        sn = ZERO;
      } else {
        temp = A - D;
        p = HALF * temp;
        bcmax = Math.max(Math.abs(B), Math.abs(C));
        bcmis = Math.min(Math.abs(B), Math.abs(C)) * sign(ONE, B) * sign(ONE, C);
        scale = Math.max(Math.abs(p), bcmax);
        z = p / scale * p + bcmax / scale * bcmis;
        if (z >= MULTPL * EPS) {
          z = p + sign(Math.sqrt(scale) * Math.sqrt(z), p);
          A = D + z;
          D = D - bcmax / z * bcmis;
          tau = dlapy2(C, z);
          cs = z / tau;
          sn = C / tau;
          B = B - C;
          C = ZERO;
        } else {
          count = 0;
          sigma = B + C;
          while (true) {
            count += 1;
            scale = Math.max(Math.abs(temp), Math.abs(sigma));
            if (scale >= SAFMX2) {
              sigma = sigma * SAFMN2;
              temp = temp * SAFMN2;
              if (count <= 20) {
                continue;
              }
            }
            if (scale <= SAFMN2) {
              sigma = sigma * SAFMX2;
              temp = temp * SAFMX2;
              if (count <= 20) {
                continue;
              }
            }
            break;
          }
          p = HALF * temp;
          tau = dlapy2(sigma, temp);
          cs = Math.sqrt(HALF * (ONE + Math.abs(sigma) / tau));
          sn = -(p / (tau * cs)) * sign(ONE, sigma);
          aa = A * cs + B * sn;
          bb = -A * sn + B * cs;
          cc = C * cs + D * sn;
          dd = -C * sn + D * cs;
          A = aa * cs + cc * sn;
          B = bb * cs + dd * sn;
          C = -aa * sn + cc * cs;
          D = -bb * sn + dd * cs;
          temp = HALF * (A + D);
          A = temp;
          D = temp;
          if (C !== ZERO) {
            if (B !== ZERO) {
              if (sign(ONE, B) === sign(ONE, C)) {
                sab = Math.sqrt(Math.abs(B));
                sac = Math.sqrt(Math.abs(C));
                p = sign(sab * sac, C);
                tau = ONE / Math.sqrt(Math.abs(B + C));
                A = temp + p;
                D = temp - p;
                B = B - C;
                C = ZERO;
                cs1 = sab * tau;
                sn1 = sac * tau;
                temp = cs * cs1 - sn * sn1;
                sn = cs * sn1 + sn * cs1;
                cs = temp;
              }
            } else {
              B = -C;
              C = ZERO;
              temp = cs;
              cs = -sn;
              sn = temp;
            }
          }
        }
      }
      rt1r = A;
      rt2r = D;
      if (C === ZERO) {
        rt1i = ZERO;
        rt2i = ZERO;
      } else {
        rt1i = Math.sqrt(Math.abs(B)) * Math.sqrt(Math.abs(C));
        rt2i = -rt1i;
      }
      return {
        "a": A,
        "b": B,
        "c": C,
        "d": D,
        "rt1r": rt1r,
        "rt1i": rt1i,
        "rt2r": rt2r,
        "rt2i": rt2i,
        "cs": cs,
        "sn": sn
      };
    }
    module.exports = dlanv2;
  }
});

// lib/lapack/base/dlahqr/lib/base.js
var require_base34 = __commonJS({
  "lib/lapack/base/dlahqr/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlanv2 = require_base33();
    var dlarfg = require_base20();
    var drot = require_base4();
    var dcopy = require_base23();
    var ZERO = 0;
    var ONE = 1;
    var TWO = 2;
    var DAT1 = 3 / 4;
    var DAT2 = -0.4375;
    var KEXSH = 10;
    var SAFMIN = dlamch("safe-minimum");
    var SAFMAX = ONE / SAFMIN;
    var ULP = dlamch("precision");
    function dlahqr(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ) {
      var rtdisc;
      var smlnum;
      var kdefl;
      var itmax;
      var info;
      var h11;
      var h12;
      var h21;
      var h21s;
      var h22;
      var rt1r;
      var rt1i;
      var rt2r;
      var rt2i;
      var lanv2;
      var sum;
      var tst;
      var aa;
      var ab;
      var ba;
      var bb;
      var cs;
      var sn;
      var det;
      var nr;
      var nh;
      var nz;
      var tr;
      var t1;
      var t2;
      var t3;
      var v2;
      var v3;
      var s;
      var v;
      var tau;
      var i;
      var i1;
      var i2;
      var its;
      var j;
      var k;
      var l;
      var m;
      v = new Float64Array(3);
      tau = new Float64Array(1);
      info = 0;
      if (N === 0) {
        return info;
      }
      if (ilo === ihi) {
        WR[offsetWR + (ilo - 1) * strideWR] = H[offsetH + (ilo - 1) * strideH1 + (ilo - 1) * strideH2];
        WI[offsetWI + (ilo - 1) * strideWI] = ZERO;
        return info;
      }
      for (j = ilo; j <= ihi - 3; j++) {
        H[offsetH + (j + 1) * strideH1 + (j - 1) * strideH2] = ZERO;
        H[offsetH + (j + 1 + 1) * strideH1 + (j - 1) * strideH2] = ZERO;
      }
      if (ilo <= ihi - 2) {
        H[offsetH + (ihi - 1) * strideH1 + (ihi - 2 - 1) * strideH2] = ZERO;
      }
      nh = ihi - ilo + 1;
      nz = ihiz - iloz + 1;
      smlnum = SAFMIN * (nh / ULP);
      if (wantt) {
        i1 = 1;
        i2 = N;
      }
      itmax = 30 * Math.max(10, nh);
      kdefl = 0;
      i = ihi;
      while (i >= ilo) {
        l = ilo;
        var converged = false;
        for (its = 0; its <= itmax; its++) {
          for (k = i; k >= l + 1; k--) {
            if (Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2]) <= smlnum) {
              break;
            }
            tst = Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 1 - 1) * strideH2]) + Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1) * strideH2]);
            if (tst === ZERO) {
              if (k - 2 >= ilo) {
                tst = tst + Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 2 - 1) * strideH2]);
              }
              if (k + 1 <= ihi) {
                tst = tst + Math.abs(H[offsetH + (k + 1 - 1) * strideH1 + (k - 1) * strideH2]);
              }
            }
            if (Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2]) <= ULP * tst) {
              ab = Math.max(Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2]), Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 1) * strideH2]));
              ba = Math.min(Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2]), Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 1) * strideH2]));
              aa = Math.max(Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1) * strideH2]), Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 1 - 1) * strideH2] - H[offsetH + (k - 1) * strideH1 + (k - 1) * strideH2]));
              bb = Math.min(Math.abs(H[offsetH + (k - 1) * strideH1 + (k - 1) * strideH2]), Math.abs(H[offsetH + (k - 1 - 1) * strideH1 + (k - 1 - 1) * strideH2] - H[offsetH + (k - 1) * strideH1 + (k - 1) * strideH2]));
              s = aa + ab;
              if (ba * (ab / s) <= Math.max(smlnum, ULP * (bb * (aa / s)))) {
                break;
              }
            }
          }
          l = k;
          if (l > ilo) {
            H[offsetH + (l - 1) * strideH1 + (l - 1 - 1) * strideH2] = ZERO;
          }
          if (l >= i - 1) {
            converged = true;
            break;
          }
          kdefl += 1;
          if (!wantt) {
            i1 = l;
            i2 = i;
          }
          if (kdefl % (2 * KEXSH) === 0) {
            s = Math.abs(H[offsetH + (i - 1) * strideH1 + (i - 1 - 1) * strideH2]) + Math.abs(H[offsetH + (i - 1 - 1) * strideH1 + (i - 2 - 1) * strideH2]);
            h11 = DAT1 * s + H[offsetH + (i - 1) * strideH1 + (i - 1) * strideH2];
            h12 = DAT2 * s;
            h21 = s;
            h22 = h11;
          } else if (kdefl % KEXSH === 0) {
            s = Math.abs(H[offsetH + (l + 1 - 1) * strideH1 + (l - 1) * strideH2]) + Math.abs(H[offsetH + (l + 2 - 1) * strideH1 + (l + 1 - 1) * strideH2]);
            h11 = DAT1 * s + H[offsetH + (l - 1) * strideH1 + (l - 1) * strideH2];
            h12 = DAT2 * s;
            h21 = s;
            h22 = h11;
          } else {
            h11 = H[offsetH + (i - 1 - 1) * strideH1 + (i - 1 - 1) * strideH2];
            h21 = H[offsetH + (i - 1) * strideH1 + (i - 1 - 1) * strideH2];
            h12 = H[offsetH + (i - 1 - 1) * strideH1 + (i - 1) * strideH2];
            h22 = H[offsetH + (i - 1) * strideH1 + (i - 1) * strideH2];
          }
          s = Math.abs(h11) + Math.abs(h12) + Math.abs(h21) + Math.abs(h22);
          if (s === ZERO) {
            rt1r = ZERO;
            rt1i = ZERO;
            rt2r = ZERO;
            rt2i = ZERO;
          } else {
            h11 = h11 / s;
            h21 = h21 / s;
            h12 = h12 / s;
            h22 = h22 / s;
            tr = (h11 + h22) / TWO;
            det = (h11 - tr) * (h22 - tr) - h12 * h21;
            rtdisc = Math.sqrt(Math.abs(det));
            if (det >= ZERO) {
              rt1r = tr * s;
              rt2r = rt1r;
              rt1i = rtdisc * s;
              rt2i = -rt1i;
            } else {
              rt1r = tr + rtdisc;
              rt2r = tr - rtdisc;
              if (Math.abs(rt1r - h22) <= Math.abs(rt2r - h22)) {
                rt1r = rt1r * s;
                rt2r = rt1r;
              } else {
                rt2r = rt2r * s;
                rt1r = rt2r;
              }
              rt1i = ZERO;
              rt2i = ZERO;
            }
          }
          for (m = i - 2; m >= l; m--) {
            h21s = H[offsetH + (m + 1 - 1) * strideH1 + (m - 1) * strideH2];
            s = Math.abs(H[offsetH + (m - 1) * strideH1 + (m - 1) * strideH2] - rt2r) + Math.abs(rt2i) + Math.abs(h21s);
            h21s = H[offsetH + (m + 1 - 1) * strideH1 + (m - 1) * strideH2] / s;
            v[0] = h21s * H[offsetH + (m - 1) * strideH1 + (m + 1 - 1) * strideH2] + (H[offsetH + (m - 1) * strideH1 + (m - 1) * strideH2] - rt1r) * ((H[offsetH + (m - 1) * strideH1 + (m - 1) * strideH2] - rt2r) / s) - rt1i * (rt2i / s);
            v[1] = h21s * (H[offsetH + (m - 1) * strideH1 + (m - 1) * strideH2] + H[offsetH + (m + 1 - 1) * strideH1 + (m + 1 - 1) * strideH2] - rt1r - rt2r);
            v[2] = h21s * H[offsetH + (m + 2 - 1) * strideH1 + (m + 1 - 1) * strideH2];
            s = Math.abs(v[0]) + Math.abs(v[1]) + Math.abs(v[2]);
            v[0] = v[0] / s;
            v[1] = v[1] / s;
            v[2] = v[2] / s;
            if (m === l) {
              break;
            }
            if (Math.abs(H[offsetH + (m - 1) * strideH1 + (m - 1 - 1) * strideH2]) * (Math.abs(v[1]) + Math.abs(v[2])) <= ULP * Math.abs(v[0]) * (Math.abs(H[offsetH + (m - 1 - 1) * strideH1 + (m - 1 - 1) * strideH2]) + Math.abs(H[offsetH + (m - 1) * strideH1 + (m - 1) * strideH2]) + Math.abs(H[offsetH + (m + 1 - 1) * strideH1 + (m + 1 - 1) * strideH2]))) {
              break;
            }
          }
          for (k = m; k <= i - 1; k++) {
            nr = Math.min(3, i - k + 1);
            if (k > m) {
              dcopy(nr, H, strideH1, offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2, v, 1, 0);
            }
            dlarfg(nr, v, 0, v, 1, 1, tau, 0);
            t1 = tau[0];
            if (k > m) {
              H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2] = v[0];
              H[offsetH + (k + 1 - 1) * strideH1 + (k - 1 - 1) * strideH2] = ZERO;
              if (k < i - 1) {
                H[offsetH + (k + 2 - 1) * strideH1 + (k - 1 - 1) * strideH2] = ZERO;
              }
            } else if (m > l) {
              H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2] = H[offsetH + (k - 1) * strideH1 + (k - 1 - 1) * strideH2] * (ONE - t1);
            }
            v2 = v[1];
            t2 = t1 * v2;
            if (nr === 3) {
              v3 = v[2];
              t3 = t1 * v3;
              for (j = k; j <= i2; j++) {
                sum = H[offsetH + (k - 1) * strideH1 + (j - 1) * strideH2] + v2 * H[offsetH + (k + 1 - 1) * strideH1 + (j - 1) * strideH2] + v3 * H[offsetH + (k + 2 - 1) * strideH1 + (j - 1) * strideH2];
                H[offsetH + (k - 1) * strideH1 + (j - 1) * strideH2] -= sum * t1;
                H[offsetH + (k + 1 - 1) * strideH1 + (j - 1) * strideH2] -= sum * t2;
                H[offsetH + (k + 2 - 1) * strideH1 + (j - 1) * strideH2] -= sum * t3;
              }
              for (j = i1; j <= Math.min(k + 3, i); j++) {
                sum = H[offsetH + (j - 1) * strideH1 + (k - 1) * strideH2] + v2 * H[offsetH + (j - 1) * strideH1 + (k + 1 - 1) * strideH2] + v3 * H[offsetH + (j - 1) * strideH1 + (k + 2 - 1) * strideH2];
                H[offsetH + (j - 1) * strideH1 + (k - 1) * strideH2] -= sum * t1;
                H[offsetH + (j - 1) * strideH1 + (k + 1 - 1) * strideH2] -= sum * t2;
                H[offsetH + (j - 1) * strideH1 + (k + 2 - 1) * strideH2] -= sum * t3;
              }
              if (wantz) {
                for (j = iloz; j <= ihiz; j++) {
                  sum = Z[offsetZ + (j - 1) * strideZ1 + (k - 1) * strideZ2] + v2 * Z[offsetZ + (j - 1) * strideZ1 + (k + 1 - 1) * strideZ2] + v3 * Z[offsetZ + (j - 1) * strideZ1 + (k + 2 - 1) * strideZ2];
                  Z[offsetZ + (j - 1) * strideZ1 + (k - 1) * strideZ2] -= sum * t1;
                  Z[offsetZ + (j - 1) * strideZ1 + (k + 1 - 1) * strideZ2] -= sum * t2;
                  Z[offsetZ + (j - 1) * strideZ1 + (k + 2 - 1) * strideZ2] -= sum * t3;
                }
              }
            } else if (nr === 2) {
              for (j = k; j <= i2; j++) {
                sum = H[offsetH + (k - 1) * strideH1 + (j - 1) * strideH2] + v2 * H[offsetH + (k + 1 - 1) * strideH1 + (j - 1) * strideH2];
                H[offsetH + (k - 1) * strideH1 + (j - 1) * strideH2] -= sum * t1;
                H[offsetH + (k + 1 - 1) * strideH1 + (j - 1) * strideH2] -= sum * t2;
              }
              for (j = i1; j <= i; j++) {
                sum = H[offsetH + (j - 1) * strideH1 + (k - 1) * strideH2] + v2 * H[offsetH + (j - 1) * strideH1 + (k + 1 - 1) * strideH2];
                H[offsetH + (j - 1) * strideH1 + (k - 1) * strideH2] -= sum * t1;
                H[offsetH + (j - 1) * strideH1 + (k + 1 - 1) * strideH2] -= sum * t2;
              }
              if (wantz) {
                for (j = iloz; j <= ihiz; j++) {
                  sum = Z[offsetZ + (j - 1) * strideZ1 + (k - 1) * strideZ2] + v2 * Z[offsetZ + (j - 1) * strideZ1 + (k + 1 - 1) * strideZ2];
                  Z[offsetZ + (j - 1) * strideZ1 + (k - 1) * strideZ2] -= sum * t1;
                  Z[offsetZ + (j - 1) * strideZ1 + (k + 1 - 1) * strideZ2] -= sum * t2;
                }
              }
            }
          }
        }
        if (!converged) {
          info = i;
          return info;
        }
        if (l === i) {
          WR[offsetWR + (i - 1) * strideWR] = H[offsetH + (i - 1) * strideH1 + (i - 1) * strideH2];
          WI[offsetWI + (i - 1) * strideWI] = ZERO;
        } else if (l === i - 1) {
          lanv2 = dlanv2(
            H[offsetH + (i - 1 - 1) * strideH1 + (i - 1 - 1) * strideH2],
            H[offsetH + (i - 1 - 1) * strideH1 + (i - 1) * strideH2],
            H[offsetH + (i - 1) * strideH1 + (i - 1 - 1) * strideH2],
            H[offsetH + (i - 1) * strideH1 + (i - 1) * strideH2]
          );
          H[offsetH + (i - 1 - 1) * strideH1 + (i - 1 - 1) * strideH2] = lanv2.a;
          H[offsetH + (i - 1 - 1) * strideH1 + (i - 1) * strideH2] = lanv2.b;
          H[offsetH + (i - 1) * strideH1 + (i - 1 - 1) * strideH2] = lanv2.c;
          H[offsetH + (i - 1) * strideH1 + (i - 1) * strideH2] = lanv2.d;
          WR[offsetWR + (i - 1 - 1) * strideWR] = lanv2.rt1r;
          WI[offsetWI + (i - 1 - 1) * strideWI] = lanv2.rt1i;
          WR[offsetWR + (i - 1) * strideWR] = lanv2.rt2r;
          WI[offsetWI + (i - 1) * strideWI] = lanv2.rt2i;
          cs = lanv2.cs;
          sn = lanv2.sn;
          if (wantt) {
            if (i2 > i) {
              drot(i2 - i, H, strideH2, offsetH + (i - 1 - 1) * strideH1 + (i + 1 - 1) * strideH2, H, strideH2, offsetH + (i - 1) * strideH1 + (i + 1 - 1) * strideH2, cs, sn);
            }
            drot(i - i1 - 1, H, strideH1, offsetH + (i1 - 1) * strideH1 + (i - 1 - 1) * strideH2, H, strideH1, offsetH + (i1 - 1) * strideH1 + (i - 1) * strideH2, cs, sn);
          }
          if (wantz) {
            drot(nz, Z, strideZ1, offsetZ + (iloz - 1) * strideZ1 + (i - 1 - 1) * strideZ2, Z, strideZ1, offsetZ + (iloz - 1) * strideZ1 + (i - 1) * strideZ2, cs, sn);
          }
        }
        kdefl = 0;
        i = l - 1;
      }
      return info;
    }
    module.exports = dlahqr;
  }
});

// lib/lapack/base/dlaset/lib/base.js
var require_base35 = __commonJS({
  "lib/lapack/base/dlaset/lib/base.js"(exports, module) {
    "use strict";
    function dlaset(uplo, M, N, alpha, beta, A, strideA1, strideA2, offsetA) {
      var idx;
      var mn;
      var i;
      var j;
      mn = Math.min(M, N);
      if (uplo === "upper") {
        for (j = 1; j < N; j++) {
          idx = offsetA + j * strideA2;
          for (i = 0; i < Math.min(j, M); i++) {
            A[idx] = alpha;
            idx += strideA1;
          }
        }
      } else if (uplo === "lower") {
        for (j = 0; j < mn; j++) {
          idx = offsetA + (j + 1) * strideA1 + j * strideA2;
          for (i = j + 1; i < M; i++) {
            A[idx] = alpha;
            idx += strideA1;
          }
        }
      } else {
        for (j = 0; j < N; j++) {
          idx = offsetA + j * strideA2;
          for (i = 0; i < M; i++) {
            A[idx] = alpha;
            idx += strideA1;
          }
        }
      }
      idx = offsetA;
      for (i = 0; i < mn; i++) {
        A[idx] = beta;
        idx += strideA1 + strideA2;
      }
      return A;
    }
    module.exports = dlaset;
  }
});

// lib/lapack/base/dorm2r/lib/base.js
var require_base36 = __commonJS({
  "lib/lapack/base/dorm2r/lib/base.js"(exports, module) {
    "use strict";
    var dlarf = require_base18();
    function dorm2r(side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var notran;
      var left;
      var idxA;
      var aii;
      var mi;
      var ni;
      var ic;
      var jc;
      var i1;
      var i2;
      var i3;
      var i;
      if (M === 0 || N === 0 || K === 0) {
        return 0;
      }
      left = side === "left";
      notran = trans === "no-transpose";
      if (left && !notran || !left && notran) {
        i1 = 0;
        i2 = K;
        i3 = 1;
      } else {
        i1 = K - 1;
        i2 = -1;
        i3 = -1;
      }
      if (left) {
        ni = N;
        jc = 0;
      } else {
        mi = M;
        ic = 0;
      }
      for (i = i1; i !== i2; i += i3) {
        if (left) {
          mi = M - i;
          ic = i;
        } else {
          ni = N - i;
          jc = i;
        }
        idxA = offsetA + i * strideA1 + i * strideA2;
        aii = A[idxA];
        A[idxA] = 1;
        dlarf(
          side,
          mi,
          ni,
          A,
          strideA1,
          offsetA + i * strideA1 + i * strideA2,
          TAU[offsetTAU + i * strideTAU],
          C,
          strideC1,
          strideC2,
          offsetC + ic * strideC1 + jc * strideC2,
          WORK,
          strideWORK,
          offsetWORK
        );
        A[idxA] = aii;
      }
      return 0;
    }
    module.exports = dorm2r;
  }
});

// lib/lapack/base/dormqr/lib/base.js
var require_base37 = __commonJS({
  "lib/lapack/base/dormqr/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dlarfb = require_base27();
    var dlarft = require_base30();
    var dorm2r = require_base36();
    var NB = 32;
    function dormqr(side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var notran;
      var ldwork;
      var left;
      var ldt;
      var nw;
      var nb;
      var nq;
      var mi;
      var ni;
      var ic;
      var jc;
      var ib;
      var i1;
      var i2;
      var i3;
      var T;
      var i;
      if (M === 0 || N === 0 || K === 0) {
        return 0;
      }
      left = side === "left";
      notran = trans === "no-transpose";
      if (left) {
        nq = M;
        nw = Math.max(1, N);
      } else {
        nq = N;
        nw = Math.max(1, M);
      }
      nb = NB;
      if (nb > K) {
        nb = K;
      }
      if (nb < 2 || nb >= K) {
        return dorm2r(side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK);
      }
      ldwork = nw;
      ldt = nb + 1;
      T = new Float64Array2(ldt * nb);
      if (!WORK || WORK.length < nw * nb + ldt * nb) {
        WORK = new Float64Array2(nw * nb + ldt * nb);
        offsetWORK = 0;
        strideWORK = 1;
      }
      if (left && !notran || !left && notran) {
        i1 = 0;
        i2 = K;
        i3 = nb;
      } else {
        i1 = Math.floor((K - 1) / nb) * nb;
        i2 = -1;
        i3 = -nb;
      }
      if (left) {
        ni = N;
        jc = 0;
      } else {
        mi = M;
        ic = 0;
      }
      for (i = i1; i3 > 0 ? i < i2 : i > i2; i += i3) {
        ib = Math.min(nb, K - i);
        dlarft(
          "forward",
          "columnwise",
          nq - i,
          ib,
          A,
          strideA1,
          strideA2,
          offsetA + i * strideA1 + i * strideA2,
          TAU,
          strideTAU,
          offsetTAU + i * strideTAU,
          T,
          1,
          ldt,
          0
        );
        if (left) {
          mi = M - i;
          ic = i;
        } else {
          ni = N - i;
          jc = i;
        }
        dlarfb(
          side,
          trans,
          "forward",
          "columnwise",
          mi,
          ni,
          ib,
          A,
          strideA1,
          strideA2,
          offsetA + i * strideA1 + i * strideA2,
          T,
          1,
          ldt,
          0,
          C,
          strideC1,
          strideC2,
          offsetC + ic * strideC1 + jc * strideC2,
          WORK,
          1,
          ldwork,
          offsetWORK
        );
      }
      return 0;
    }
    module.exports = dormqr;
  }
});

// lib/lapack/base/dormhr/lib/base.js
var require_base38 = __commonJS({
  "lib/lapack/base/dormhr/lib/base.js"(exports, module) {
    "use strict";
    var dormqr = require_base37();
    function dormhr(side, trans, M, N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork) {
      var left;
      var nh;
      var mi;
      var ni;
      var ic;
      var jc;
      nh = ihi - ilo;
      left = side === "left";
      if (M === 0 || N === 0 || nh === 0) {
        return 0;
      }
      if (left) {
        mi = nh;
        ni = N;
        ic = ilo;
        jc = 0;
      } else {
        mi = M;
        ni = nh;
        ic = 0;
        jc = ilo;
      }
      return dormqr(
        side,
        trans,
        mi,
        ni,
        nh,
        A,
        strideA1,
        strideA2,
        offsetA + ilo * strideA1 + (ilo - 1) * strideA2,
        TAU,
        strideTAU,
        offsetTAU + (ilo - 1) * strideTAU,
        C,
        strideC1,
        strideC2,
        offsetC + ic * strideC1 + jc * strideC2,
        WORK,
        strideWORK,
        offsetWORK
      );
    }
    module.exports = dormhr;
  }
});

// lib/lapack/base/dlarfx/lib/base.js
var require_base39 = __commonJS({
  "lib/lapack/base/dlarfx/lib/base.js"(exports, module) {
    "use strict";
    var dlarf = require_base18();
    var ZERO = 0;
    function dlarfx(side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var sum;
      var vv;
      var tt;
      var i;
      var j;
      var dim;
      if (tau === ZERO) {
        return;
      }
      if (side === "left") {
        dim = M;
        if (dim >= 1 && dim <= 10) {
          vv = new Float64Array(dim);
          tt = new Float64Array(dim);
          for (i = 0; i < dim; i++) {
            vv[i] = v[offsetV + i * strideV];
            tt[i] = tau * vv[i];
          }
          for (j = 0; j < N; j++) {
            sum = 0;
            for (i = 0; i < dim; i++) {
              sum += vv[i] * C[offsetC + i * strideC1 + j * strideC2];
            }
            for (i = 0; i < dim; i++) {
              C[offsetC + i * strideC1 + j * strideC2] -= sum * tt[i];
            }
          }
          return;
        }
        dlarf("left", M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK);
      } else {
        dim = N;
        if (dim >= 1 && dim <= 10) {
          vv = new Float64Array(dim);
          tt = new Float64Array(dim);
          for (i = 0; i < dim; i++) {
            vv[i] = v[offsetV + i * strideV];
            tt[i] = tau * vv[i];
          }
          for (j = 0; j < M; j++) {
            sum = 0;
            for (i = 0; i < dim; i++) {
              sum += vv[i] * C[offsetC + j * strideC1 + i * strideC2];
            }
            for (i = 0; i < dim; i++) {
              C[offsetC + j * strideC1 + i * strideC2] -= sum * tt[i];
            }
          }
          return;
        }
        dlarf("right", M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK);
      }
    }
    module.exports = dlarfx;
  }
});

// lib/lapack/base/dlartg/lib/base.js
var require_base40 = __commonJS({
  "lib/lapack/base/dlartg/lib/base.js"(exports, module) {
    "use strict";
    var SAFMIN = 22250738585072014e-324;
    var SAFMAX = 449423283715579e293;
    var RTMIN = Math.sqrt(SAFMIN);
    var RTMAX = Math.sqrt(SAFMAX / 2);
    function dlartg(f, g, out) {
      var f1;
      var g1;
      var fs;
      var gs;
      var d;
      var u;
      f1 = Math.abs(f);
      g1 = Math.abs(g);
      if (g === 0) {
        out[0] = 1;
        out[1] = 0;
        out[2] = f;
      } else if (f === 0) {
        out[0] = 0;
        out[1] = g > 0 ? 1 : -1;
        out[2] = g1;
      } else if (f1 > RTMIN && f1 < RTMAX && g1 > RTMIN && g1 < RTMAX) {
        d = Math.sqrt(f * f + g * g);
        out[0] = f1 / d;
        out[2] = f > 0 ? d : -d;
        out[1] = g / out[2];
      } else {
        u = Math.min(SAFMAX, Math.max(SAFMIN, f1, g1));
        fs = f / u;
        gs = g / u;
        d = Math.sqrt(fs * fs + gs * gs);
        out[0] = Math.abs(fs) / d;
        out[2] = f > 0 ? d * u : -(d * u);
        out[1] = gs / (f > 0 ? d : -d);
      }
      return out;
    }
    module.exports = dlartg;
  }
});

// lib/lapack/base/dlasy2/lib/base.js
var require_base41 = __commonJS({
  "lib/lapack/base/dlasy2/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var idamax = require_base3();
    var dcopy = require_base23();
    var dswap = require_base10();
    var ZERO = 0;
    var ONE = 1;
    var TWO = 2;
    var HALF = 0.5;
    var EIGHT = 8;
    var EPS = dlamch("precision");
    var SMLNUM = dlamch("safe-minimum") / EPS;
    var LOCU12 = [2, 3, 0, 1];
    var LOCL21 = [1, 0, 3, 2];
    var LOCU22 = [3, 2, 1, 0];
    var XSWPIV = [false, false, true, true];
    var BSWPIV = [false, true, false, true];
    function dlasy2(ltranl, ltranr, isgn, n1, n2, TL, strideTL1, strideTL2, offsetTL, TR, strideTR1, strideTR2, offsetTR, B, strideB1, strideB2, offsetB, scale, X, strideX1, strideX2, offsetX, xnorm) {
      var info;
      var bswap;
      var xswap;
      var smin;
      var temp;
      var ipiv;
      var ipsv;
      var jpsv;
      var xmax;
      var tau1;
      var bet;
      var gam;
      var sgn;
      var u11;
      var u12;
      var u22;
      var l21;
      var btmp;
      var tmp;
      var x2;
      var t16;
      var jpiv;
      var k;
      var i;
      var j;
      var ip;
      var jp;
      info = 0;
      if (n1 === 0 || n2 === 0) {
        return info;
      }
      sgn = isgn;
      k = n1 + n1 + n2 - 2;
      if (k === 1) {
        tau1 = TL[offsetTL] + sgn * TR[offsetTR];
        bet = Math.abs(tau1);
        if (bet <= SMLNUM) {
          tau1 = SMLNUM;
          bet = SMLNUM;
          info = 1;
        }
        scale[0] = ONE;
        gam = Math.abs(B[offsetB]);
        if (SMLNUM * gam > bet) {
          scale[0] = ONE / gam;
        }
        X[offsetX] = B[offsetB] * scale[0] / tau1;
        xnorm[0] = Math.abs(X[offsetX]);
        return info;
      }
      if (k === 2) {
        smin = Math.max(
          EPS * Math.max(
            Math.abs(TL[offsetTL]),
            Math.abs(TR[offsetTR]),
            Math.abs(TR[offsetTR + strideTR2]),
            Math.abs(TR[offsetTR + strideTR1]),
            Math.abs(TR[offsetTR + strideTR1 + strideTR2])
          ),
          SMLNUM
        );
        tmp = new Float64Array(4);
        tmp[0] = TL[offsetTL] + sgn * TR[offsetTR];
        tmp[3] = TL[offsetTL] + sgn * TR[offsetTR + strideTR1 + strideTR2];
        if (ltranr) {
          tmp[1] = sgn * TR[offsetTR + strideTR1];
          tmp[2] = sgn * TR[offsetTR + strideTR2];
        } else {
          tmp[1] = sgn * TR[offsetTR + strideTR2];
          tmp[2] = sgn * TR[offsetTR + strideTR1];
        }
        btmp = new Float64Array(4);
        btmp[0] = B[offsetB];
        btmp[1] = B[offsetB + strideB2];
        return solve2x2(tmp, btmp, smin, n1, scale, X, strideX1, strideX2, offsetX, xnorm);
      }
      if (k === 3) {
        smin = Math.max(
          EPS * Math.max(
            Math.abs(TR[offsetTR]),
            Math.abs(TL[offsetTL]),
            Math.abs(TL[offsetTL + strideTL2]),
            Math.abs(TL[offsetTL + strideTL1]),
            Math.abs(TL[offsetTL + strideTL1 + strideTL2])
          ),
          SMLNUM
        );
        tmp = new Float64Array(4);
        tmp[0] = TL[offsetTL] + sgn * TR[offsetTR];
        tmp[3] = TL[offsetTL + strideTL1 + strideTL2] + sgn * TR[offsetTR];
        if (ltranl) {
          tmp[1] = TL[offsetTL + strideTL2];
          tmp[2] = TL[offsetTL + strideTL1];
        } else {
          tmp[1] = TL[offsetTL + strideTL1];
          tmp[2] = TL[offsetTL + strideTL2];
        }
        btmp = new Float64Array(4);
        btmp[0] = B[offsetB];
        btmp[1] = B[offsetB + strideB1];
        return solve2x2(tmp, btmp, smin, n1, scale, X, strideX1, strideX2, offsetX, xnorm);
      }
      smin = Math.max(
        Math.abs(TR[offsetTR]),
        Math.abs(TR[offsetTR + strideTR2]),
        Math.abs(TR[offsetTR + strideTR1]),
        Math.abs(TR[offsetTR + strideTR1 + strideTR2])
      );
      smin = Math.max(
        smin,
        Math.abs(TL[offsetTL]),
        Math.abs(TL[offsetTL + strideTL2]),
        Math.abs(TL[offsetTL + strideTL1]),
        Math.abs(TL[offsetTL + strideTL1 + strideTL2])
      );
      smin = Math.max(EPS * smin, SMLNUM);
      btmp = new Float64Array(4);
      t16 = new Float64Array(16);
      t16[0] = TL[offsetTL] + sgn * TR[offsetTR];
      t16[1 + 4] = TL[offsetTL + strideTL1 + strideTL2] + sgn * TR[offsetTR];
      t16[2 + 2 * 4] = TL[offsetTL] + sgn * TR[offsetTR + strideTR1 + strideTR2];
      t16[3 + 3 * 4] = TL[offsetTL + strideTL1 + strideTL2] + sgn * TR[offsetTR + strideTR1 + strideTR2];
      if (ltranl) {
        t16[0 + 1 * 4] = TL[offsetTL + strideTL1];
        t16[1 + 0 * 4] = TL[offsetTL + strideTL2];
        t16[2 + 3 * 4] = TL[offsetTL + strideTL1];
        t16[3 + 2 * 4] = TL[offsetTL + strideTL2];
      } else {
        t16[0 + 1 * 4] = TL[offsetTL + strideTL2];
        t16[1 + 0 * 4] = TL[offsetTL + strideTL1];
        t16[2 + 3 * 4] = TL[offsetTL + strideTL2];
        t16[3 + 2 * 4] = TL[offsetTL + strideTL1];
      }
      if (ltranr) {
        t16[0 + 2 * 4] = sgn * TR[offsetTR + strideTR2];
        t16[1 + 3 * 4] = sgn * TR[offsetTR + strideTR2];
        t16[2 + 0 * 4] = sgn * TR[offsetTR + strideTR1];
        t16[3 + 1 * 4] = sgn * TR[offsetTR + strideTR1];
      } else {
        t16[0 + 2 * 4] = sgn * TR[offsetTR + strideTR1];
        t16[1 + 3 * 4] = sgn * TR[offsetTR + strideTR1];
        t16[2 + 0 * 4] = sgn * TR[offsetTR + strideTR2];
        t16[3 + 1 * 4] = sgn * TR[offsetTR + strideTR2];
      }
      btmp[0] = B[offsetB];
      btmp[1] = B[offsetB + strideB1];
      btmp[2] = B[offsetB + strideB2];
      btmp[3] = B[offsetB + strideB1 + strideB2];
      jpiv = new Int32Array(4);
      for (i = 0; i < 3; i++) {
        xmax = ZERO;
        ipsv = i;
        jpsv = i;
        for (ip = i; ip < 4; ip++) {
          for (jp = i; jp < 4; jp++) {
            if (Math.abs(t16[ip + jp * 4]) >= xmax) {
              xmax = Math.abs(t16[ip + jp * 4]);
              ipsv = ip;
              jpsv = jp;
            }
          }
        }
        if (ipsv !== i) {
          dswap(4, t16, 4, ipsv, t16, 4, i);
          temp = btmp[i];
          btmp[i] = btmp[ipsv];
          btmp[ipsv] = temp;
        }
        if (jpsv !== i) {
          dswap(4, t16, 1, jpsv * 4, t16, 1, i * 4);
        }
        jpiv[i] = jpsv;
        if (Math.abs(t16[i + i * 4]) < smin) {
          info = 1;
          t16[i + i * 4] = smin;
        }
        for (j = i + 1; j < 4; j++) {
          t16[j + i * 4] = t16[j + i * 4] / t16[i + i * 4];
          btmp[j] = btmp[j] - t16[j + i * 4] * btmp[i];
          for (k = i + 1; k < 4; k++) {
            t16[j + k * 4] = t16[j + k * 4] - t16[j + i * 4] * t16[i + k * 4];
          }
        }
      }
      if (Math.abs(t16[3 + 3 * 4]) < smin) {
        info = 1;
        t16[3 + 3 * 4] = smin;
      }
      scale[0] = ONE;
      if (EIGHT * SMLNUM * Math.abs(btmp[0]) > Math.abs(t16[0]) || EIGHT * SMLNUM * Math.abs(btmp[1]) > Math.abs(t16[1 + 4]) || EIGHT * SMLNUM * Math.abs(btmp[2]) > Math.abs(t16[2 + 2 * 4]) || EIGHT * SMLNUM * Math.abs(btmp[3]) > Math.abs(t16[3 + 3 * 4])) {
        scale[0] = ONE / EIGHT / Math.max(
          Math.abs(btmp[0]),
          Math.abs(btmp[1]),
          Math.abs(btmp[2]),
          Math.abs(btmp[3])
        );
        btmp[0] = btmp[0] * scale[0];
        btmp[1] = btmp[1] * scale[0];
        btmp[2] = btmp[2] * scale[0];
        btmp[3] = btmp[3] * scale[0];
      }
      tmp = new Float64Array(4);
      for (i = 0; i < 4; i++) {
        k = 3 - i;
        temp = ONE / t16[k + k * 4];
        tmp[k] = btmp[k] * temp;
        for (j = k + 1; j < 4; j++) {
          tmp[k] = tmp[k] - temp * t16[k + j * 4] * tmp[j];
        }
      }
      for (i = 0; i < 3; i++) {
        k = 2 - i;
        if (jpiv[k] !== k) {
          temp = tmp[k];
          tmp[k] = tmp[jpiv[k]];
          tmp[jpiv[k]] = temp;
        }
      }
      X[offsetX] = tmp[0];
      X[offsetX + strideX1] = tmp[1];
      X[offsetX + strideX2] = tmp[2];
      X[offsetX + strideX1 + strideX2] = tmp[3];
      xnorm[0] = Math.max(
        Math.abs(tmp[0]) + Math.abs(tmp[2]),
        Math.abs(tmp[1]) + Math.abs(tmp[3])
      );
      return info;
    }
    function solve2x2(tmp, btmp, smin, n1, scale, X, strideX1, strideX2, offsetX, xnorm) {
      var bswap;
      var xswap;
      var ipiv;
      var info;
      var temp;
      var u11;
      var u12;
      var u22;
      var l21;
      var x2;
      info = 0;
      ipiv = idamax(4, tmp, 1, 0);
      u11 = tmp[ipiv];
      if (Math.abs(u11) <= smin) {
        info = 1;
        u11 = smin;
      }
      u12 = tmp[LOCU12[ipiv]];
      l21 = tmp[LOCL21[ipiv]] / u11;
      u22 = tmp[LOCU22[ipiv]] - u12 * l21;
      xswap = XSWPIV[ipiv];
      bswap = BSWPIV[ipiv];
      if (Math.abs(u22) <= smin) {
        info = 1;
        u22 = smin;
      }
      if (bswap) {
        temp = btmp[1];
        btmp[1] = btmp[0] - l21 * temp;
        btmp[0] = temp;
      } else {
        btmp[1] = btmp[1] - l21 * btmp[0];
      }
      scale[0] = ONE;
      if (TWO * SMLNUM * Math.abs(btmp[1]) > Math.abs(u22) || TWO * SMLNUM * Math.abs(btmp[0]) > Math.abs(u11)) {
        scale[0] = HALF / Math.max(Math.abs(btmp[0]), Math.abs(btmp[1]));
        btmp[0] = btmp[0] * scale[0];
        btmp[1] = btmp[1] * scale[0];
      }
      x2 = new Float64Array(2);
      x2[1] = btmp[1] / u22;
      x2[0] = btmp[0] / u11 - u12 / u11 * x2[1];
      if (xswap) {
        temp = x2[1];
        x2[1] = x2[0];
        x2[0] = temp;
      }
      X[offsetX] = x2[0];
      if (n1 === 1) {
        X[offsetX + strideX2] = x2[1];
        xnorm[0] = Math.abs(X[offsetX]) + Math.abs(X[offsetX + strideX2]);
      } else {
        X[offsetX + strideX1] = x2[1];
        xnorm[0] = Math.max(Math.abs(X[offsetX]), Math.abs(X[offsetX + strideX1]));
      }
      return info;
    }
    module.exports = dlasy2;
  }
});

// lib/lapack/base/dlaexc/lib/base.js
var require_base42 = __commonJS({
  "lib/lapack/base/dlaexc/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlacpy = require_base9();
    var dlange = require_base7();
    var dlanv2 = require_base33();
    var dlarfg = require_base20();
    var dlarfx = require_base39();
    var dlartg = require_base40();
    var dlasy2 = require_base41();
    var drot = require_base4();
    var ZERO = 0;
    var ONE = 1;
    var TEN = 10;
    var LDD = 4;
    var LDX = 2;
    var EPS = dlamch("precision");
    var SMLNUM = dlamch("safe-minimum") / EPS;
    function dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, j1, n1, n2, WORK, strideWORK, offsetWORK) {
      var dnorm;
      var scale;
      var xnorm;
      var thresh;
      var lanv2r;
      var rtgOut;
      var info;
      var ierr;
      var temp;
      var tau;
      var tau1;
      var tau2;
      var cs;
      var sn;
      var t11;
      var t22;
      var t33;
      var nd;
      var k;
      var j2;
      var j3;
      var j4;
      var D;
      var X;
      var u;
      var u1;
      var u2;
      info = 0;
      if (N === 0 || n1 === 0 || n2 === 0) {
        return 0;
      }
      if (j1 + n1 > N) {
        return 0;
      }
      j2 = j1 + 1;
      j3 = j1 + 2;
      j4 = j1 + 3;
      function tij(i, j) {
        return offsetT + (i - 1) * strideT1 + (j - 1) * strideT2;
      }
      function qij(i, j) {
        return offsetQ + (i - 1) * strideQ1 + (j - 1) * strideQ2;
      }
      if (n1 === 1 && n2 === 1) {
        t11 = T[tij(j1, j1)];
        t22 = T[tij(j2, j2)];
        rtgOut = new Float64Array(3);
        dlartg(T[tij(j1, j2)], t22 - t11, rtgOut);
        cs = rtgOut[0];
        sn = rtgOut[1];
        if (j3 <= N) {
          drot(N - j1 - 1, T, strideT2, tij(j1, j3), T, strideT2, tij(j2, j3), cs, sn);
        }
        drot(j1 - 1, T, strideT1, tij(1, j1), T, strideT1, tij(1, j2), cs, sn);
        T[tij(j1, j1)] = t22;
        T[tij(j2, j2)] = t11;
        if (wantq) {
          drot(N, Q, strideQ1, qij(1, j1), Q, strideQ1, qij(1, j2), cs, sn);
        }
      } else {
        nd = n1 + n2;
        D = new Float64Array(LDD * 4);
        X = new Float64Array(LDX * 2);
        dlacpy("full", nd, nd, T, strideT1, strideT2, tij(j1, j1), D, 1, LDD, 0);
        dnorm = dlange("max", nd, nd, D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
        thresh = Math.max(TEN * EPS * dnorm, SMLNUM);
        scale = new Float64Array(1);
        xnorm = new Float64Array(1);
        ierr = dlasy2(false, false, -1, n1, n2, D, 1, LDD, 0, D, 1, LDD, n1 * 1 + n1 * LDD, D, 1, LDD, n1 * LDD, scale, X, 1, LDX, 0, xnorm);
        k = n1 + n1 + n2 - 3;
        if (k === 1) {
          u = new Float64Array(3);
          tau = new Float64Array(1);
          u[0] = scale[0];
          u[1] = X[0];
          u[2] = X[LDX];
          dlarfg(3, u, 2, u, 1, 0, tau, 0);
          u[2] = ONE;
          t11 = T[tij(j1, j1)];
          dlarfx("left", 3, 3, u, 1, 0, tau[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          dlarfx("right", 3, 3, u, 1, 0, tau[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          if (Math.max(Math.abs(D[2]), Math.abs(D[2 + LDD]), Math.abs(D[2 + 2 * LDD] - t11)) > thresh) {
            return 1;
          }
          dlarfx("left", 3, N - j1 + 1, u, 1, 0, tau[0], T, strideT1, strideT2, tij(j1, j1), WORK, strideWORK, offsetWORK);
          dlarfx("right", j2, 3, u, 1, 0, tau[0], T, strideT1, strideT2, tij(1, j1), WORK, strideWORK, offsetWORK);
          T[tij(j3, j1)] = ZERO;
          T[tij(j3, j2)] = ZERO;
          T[tij(j3, j3)] = t11;
          if (wantq) {
            dlarfx("right", N, 3, u, 1, 0, tau[0], Q, strideQ1, strideQ2, qij(1, j1), WORK, strideWORK, offsetWORK);
          }
        } else if (k === 2) {
          u = new Float64Array(3);
          tau = new Float64Array(1);
          u[0] = -X[0];
          u[1] = -X[1];
          u[2] = scale[0];
          dlarfg(3, u, 0, u, 1, 1, tau, 0);
          u[0] = ONE;
          t33 = T[tij(j3, j3)];
          dlarfx("left", 3, 3, u, 1, 0, tau[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          dlarfx("right", 3, 3, u, 1, 0, tau[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          if (Math.max(Math.abs(D[1]), Math.abs(D[2]), Math.abs(D[0] - t33)) > thresh) {
            return 1;
          }
          dlarfx("right", j3, 3, u, 1, 0, tau[0], T, strideT1, strideT2, tij(1, j1), WORK, strideWORK, offsetWORK);
          dlarfx("left", 3, N - j1, u, 1, 0, tau[0], T, strideT1, strideT2, tij(j1, j2), WORK, strideWORK, offsetWORK);
          T[tij(j1, j1)] = t33;
          T[tij(j2, j1)] = ZERO;
          T[tij(j3, j1)] = ZERO;
          if (wantq) {
            dlarfx("right", N, 3, u, 1, 0, tau[0], Q, strideQ1, strideQ2, qij(1, j1), WORK, strideWORK, offsetWORK);
          }
        } else {
          u1 = new Float64Array(3);
          u2 = new Float64Array(3);
          tau1 = new Float64Array(1);
          tau2 = new Float64Array(1);
          u1[0] = -X[0];
          u1[1] = -X[1];
          u1[2] = scale[0];
          dlarfg(3, u1, 0, u1, 1, 1, tau1, 0);
          u1[0] = ONE;
          temp = -tau1[0] * (X[LDX] + u1[1] * X[1 + LDX]);
          u2[0] = -temp * u1[1] - X[1 + LDX];
          u2[1] = -temp * u1[2];
          u2[2] = scale[0];
          dlarfg(3, u2, 0, u2, 1, 1, tau2, 0);
          u2[0] = ONE;
          dlarfx("left", 3, 4, u1, 1, 0, tau1[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          dlarfx("right", 4, 3, u1, 1, 0, tau1[0], D, 1, LDD, 0, WORK, strideWORK, offsetWORK);
          dlarfx("left", 3, 4, u2, 1, 0, tau2[0], D, 1, LDD, 1, WORK, strideWORK, offsetWORK);
          dlarfx("right", 4, 3, u2, 1, 0, tau2[0], D, 1, LDD, LDD, WORK, strideWORK, offsetWORK);
          if (Math.max(Math.abs(D[2]), Math.abs(D[2 + LDD]), Math.abs(D[3]), Math.abs(D[3 + LDD])) > thresh) {
            return 1;
          }
          dlarfx("left", 3, N - j1 + 1, u1, 1, 0, tau1[0], T, strideT1, strideT2, tij(j1, j1), WORK, strideWORK, offsetWORK);
          dlarfx("right", j4, 3, u1, 1, 0, tau1[0], T, strideT1, strideT2, tij(1, j1), WORK, strideWORK, offsetWORK);
          dlarfx("left", 3, N - j1 + 1, u2, 1, 0, tau2[0], T, strideT1, strideT2, tij(j2, j1), WORK, strideWORK, offsetWORK);
          dlarfx("right", j4, 3, u2, 1, 0, tau2[0], T, strideT1, strideT2, tij(1, j2), WORK, strideWORK, offsetWORK);
          T[tij(j3, j1)] = ZERO;
          T[tij(j3, j2)] = ZERO;
          T[tij(j4, j1)] = ZERO;
          T[tij(j4, j2)] = ZERO;
          if (wantq) {
            dlarfx("right", N, 3, u1, 1, 0, tau1[0], Q, strideQ1, strideQ2, qij(1, j1), WORK, strideWORK, offsetWORK);
            dlarfx("right", N, 3, u2, 1, 0, tau2[0], Q, strideQ1, strideQ2, qij(1, j2), WORK, strideWORK, offsetWORK);
          }
        }
        if (n2 === 2) {
          lanv2r = dlanv2(
            T[tij(j1, j1)],
            T[tij(j1, j2)],
            T[tij(j2, j1)],
            T[tij(j2, j2)]
          );
          T[tij(j1, j1)] = lanv2r.a;
          T[tij(j1, j2)] = lanv2r.b;
          T[tij(j2, j1)] = lanv2r.c;
          T[tij(j2, j2)] = lanv2r.d;
          cs = lanv2r.cs;
          sn = lanv2r.sn;
          drot(N - j1 - 1, T, strideT2, tij(j1, j1 + 2), T, strideT2, tij(j2, j1 + 2), cs, sn);
          drot(j1 - 1, T, strideT1, tij(1, j1), T, strideT1, tij(1, j2), cs, sn);
          if (wantq) {
            drot(N, Q, strideQ1, qij(1, j1), Q, strideQ1, qij(1, j2), cs, sn);
          }
        }
        if (n1 === 2) {
          j3 = j1 + n2;
          j4 = j3 + 1;
          lanv2r = dlanv2(
            T[tij(j3, j3)],
            T[tij(j3, j4)],
            T[tij(j4, j3)],
            T[tij(j4, j4)]
          );
          T[tij(j3, j3)] = lanv2r.a;
          T[tij(j3, j4)] = lanv2r.b;
          T[tij(j4, j3)] = lanv2r.c;
          T[tij(j4, j4)] = lanv2r.d;
          cs = lanv2r.cs;
          sn = lanv2r.sn;
          if (j3 + 2 <= N) {
            drot(N - j3 - 1, T, strideT2, tij(j3, j3 + 2), T, strideT2, tij(j4, j3 + 2), cs, sn);
          }
          drot(j3 - 1, T, strideT1, tij(1, j3), T, strideT1, tij(1, j4), cs, sn);
          if (wantq) {
            drot(N, Q, strideQ1, qij(1, j3), Q, strideQ1, qij(1, j4), cs, sn);
          }
        }
      }
      return 0;
    }
    module.exports = dlaexc;
  }
});

// lib/lapack/base/dtrexc/lib/base.js
var require_base43 = __commonJS({
  "lib/lapack/base/dtrexc/lib/base.js"(exports, module) {
    "use strict";
    var dlaexc = require_base42();
    function dtrexc(compq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, ifst, ilst, WORK, strideWORK, offsetWORK) {
      var wantq;
      var nbnext;
      var info;
      var here;
      var nbf;
      var nbl;
      info = 0;
      wantq = compq === "update";
      if (N <= 1) {
        return { "info": 0, "ifst": ifst, "ilst": ilst };
      }
      function tij(i, j) {
        return offsetT + (i - 1) * strideT1 + (j - 1) * strideT2;
      }
      if (ifst > 1) {
        if (T[tij(ifst, ifst - 1)] !== 0) {
          ifst = ifst - 1;
        }
      }
      nbf = 1;
      if (ifst < N) {
        if (T[tij(ifst + 1, ifst)] !== 0) {
          nbf = 2;
        }
      }
      if (ilst > 1) {
        if (T[tij(ilst, ilst - 1)] !== 0) {
          ilst = ilst - 1;
        }
      }
      nbl = 1;
      if (ilst < N) {
        if (T[tij(ilst + 1, ilst)] !== 0) {
          nbl = 2;
        }
      }
      if (ifst === ilst) {
        return { "info": 0, "ifst": ifst, "ilst": ilst };
      }
      if (ifst < ilst) {
        if (nbf === 2 && nbl === 1) {
          ilst = ilst - 1;
        }
        if (nbf === 1 && nbl === 2) {
          ilst = ilst + 1;
        }
        here = ifst;
        while (here < ilst) {
          if (nbf === 1 || nbf === 2) {
            nbnext = 1;
            if (here + nbf + 1 <= N) {
              if (T[tij(here + nbf + 1, here + nbf)] !== 0) {
                nbnext = 2;
              }
            }
            info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, nbf, nbnext, WORK, strideWORK, offsetWORK);
            if (info !== 0) {
              ilst = here;
              return { "info": info, "ifst": ifst, "ilst": ilst };
            }
            here = here + nbnext;
            if (nbf === 2) {
              if (T[tij(here + 1, here)] === 0) {
                nbf = 3;
              }
            }
          } else {
            nbnext = 1;
            if (here + 3 <= N) {
              if (T[tij(here + 3, here + 2)] !== 0) {
                nbnext = 2;
              }
            }
            info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here + 1, 1, nbnext, WORK, strideWORK, offsetWORK);
            if (info !== 0) {
              ilst = here;
              return { "info": info, "ifst": ifst, "ilst": ilst };
            }
            if (nbnext === 1) {
              info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, 1, nbnext, WORK, strideWORK, offsetWORK);
              here = here + 1;
            } else {
              if (T[tij(here + 2, here + 1)] === 0) {
                nbnext = 1;
              }
              if (nbnext === 2) {
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, 1, nbnext, WORK, strideWORK, offsetWORK);
                if (info !== 0) {
                  ilst = here;
                  return { "info": info, "ifst": ifst, "ilst": ilst };
                }
                here = here + 2;
              } else {
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, 1, 1, WORK, strideWORK, offsetWORK);
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here + 1, 1, 1, WORK, strideWORK, offsetWORK);
                here = here + 2;
              }
            }
          }
        }
      } else {
        here = ifst;
        while (here > ilst) {
          if (nbf === 1 || nbf === 2) {
            nbnext = 1;
            if (here >= 3) {
              if (T[tij(here - 1, here - 2)] !== 0) {
                nbnext = 2;
              }
            }
            info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here - nbnext, nbnext, nbf, WORK, strideWORK, offsetWORK);
            if (info !== 0) {
              ilst = here;
              return { "info": info, "ifst": ifst, "ilst": ilst };
            }
            here = here - nbnext;
            if (nbf === 2) {
              if (T[tij(here + 1, here)] === 0) {
                nbf = 3;
              }
            }
          } else {
            nbnext = 1;
            if (here >= 3) {
              if (T[tij(here - 1, here - 2)] !== 0) {
                nbnext = 2;
              }
            }
            info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here - nbnext, nbnext, 1, WORK, strideWORK, offsetWORK);
            if (info !== 0) {
              ilst = here;
              return { "info": info, "ifst": ifst, "ilst": ilst };
            }
            if (nbnext === 1) {
              info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, nbnext, 1, WORK, strideWORK, offsetWORK);
              here = here - 1;
            } else {
              if (T[tij(here, here - 1)] === 0) {
                nbnext = 1;
              }
              if (nbnext === 2) {
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here - 1, 2, 1, WORK, strideWORK, offsetWORK);
                if (info !== 0) {
                  ilst = here;
                  return { "info": info, "ifst": ifst, "ilst": ilst };
                }
                here = here - 2;
              } else {
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here, 1, 1, WORK, strideWORK, offsetWORK);
                info = dlaexc(wantq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, here - 1, 1, 1, WORK, strideWORK, offsetWORK);
                here = here - 2;
              }
            }
          }
        }
      }
      ilst = here;
      return { "info": 0, "ifst": ifst, "ilst": ilst };
    }
    module.exports = dtrexc;
  }
});

// lib/lapack/base/dlaqr2/lib/base.js
var require_base44 = __commonJS({
  "lib/lapack/base/dlaqr2/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlahqr = require_base34();
    var dlacpy = require_base9();
    var dlaset = require_base35();
    var dlanv2 = require_base33();
    var dlarfg = require_base20();
    var dlarf = require_base18();
    var dgehrd = require_base28();
    var dormhr = require_base38();
    var dtrexc = require_base43();
    var dgemm2 = require_base22();
    var dcopy = require_base23();
    var ZERO = 0;
    var ONE = 1;
    var SAFMIN = dlamch("safe-minimum");
    var ULP = dlamch("precision");
    function dlaqr2(wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork) {
      return dlaqr23impl(null, wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork);
    }
    function dlaqr23impl(dlaqr4fn, wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork) {
      var lwkopt;
      var smlnum;
      var sorted;
      var kwtop;
      var infqr;
      var bulge;
      var trxResult;
      var tauArr;
      var nmin;
      var ilst;
      var ifst;
      var kend;
      var krow;
      var kcol;
      var ltop;
      var tau;
      var foo;
      var evi;
      var evk;
      var jw;
      var ns;
      var nd;
      var aa;
      var bb;
      var cc;
      var dd;
      var lv2;
      var kln;
      var lwk1;
      var lwk2;
      var lwk3;
      var s;
      var i;
      var j;
      var k;
      function hij(r, c) {
        return offsetH + (r - 1) * strideH1 + (c - 1) * strideH2;
      }
      function tij(r, c) {
        return offsetT + (r - 1) * strideT1 + (c - 1) * strideT2;
      }
      function vij(r, c) {
        return offsetV + (r - 1) * strideV1 + (c - 1) * strideV2;
      }
      function zij(r, c) {
        return offsetZ + (r - 1) * strideZ1 + (c - 1) * strideZ2;
      }
      function wvij(r, c) {
        return offsetWV + (r - 1) * strideWV1 + (c - 1) * strideWV2;
      }
      jw = Math.min(nw, kbot - ktop + 1);
      if (jw <= 2) {
        lwkopt = 1;
      } else {
        dgehrd(jw, 1, jw - 1, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK, WORK, strideWORK, offsetWORK, -1);
        lwk1 = Math.floor(WORK[offsetWORK]);
        dormhr("right", "no-transpose", jw, jw, 1, jw - 1, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK, V, strideV1, strideV2, offsetV, WORK, strideWORK, offsetWORK, -1);
        lwk2 = Math.floor(WORK[offsetWORK]);
        if (dlaqr4fn) {
          dlaqr4fn(true, true, jw, 1, jw, T, strideT1, strideT2, offsetT, SR, strideSR, offsetSR, SI, strideSI, offsetSI, 1, jw, V, strideV1, strideV2, offsetV, WORK, strideWORK, offsetWORK, -1);
          lwk3 = Math.floor(WORK[offsetWORK]);
          lwkopt = Math.max(jw + Math.max(lwk1, lwk2), lwk3);
        } else {
          lwkopt = jw + Math.max(lwk1, lwk2);
        }
      }
      if (lwork === -1) {
        WORK[offsetWORK] = lwkopt;
        return { "ns": 0, "nd": 0 };
      }
      ns = 0;
      nd = 0;
      WORK[offsetWORK] = ONE;
      if (ktop > kbot) {
        return { "ns": ns, "nd": nd };
      }
      if (nw < 1) {
        return { "ns": ns, "nd": nd };
      }
      smlnum = SAFMIN * (N / ULP);
      jw = Math.min(nw, kbot - ktop + 1);
      kwtop = kbot - jw + 1;
      if (kwtop === ktop) {
        s = ZERO;
      } else {
        s = H[hij(kwtop, kwtop - 1)];
      }
      if (kbot === kwtop) {
        SR[offsetSR + (kwtop - 1) * strideSR] = H[hij(kwtop, kwtop)];
        SI[offsetSI + (kwtop - 1) * strideSI] = ZERO;
        ns = 1;
        nd = 0;
        if (Math.abs(s) <= Math.max(smlnum, ULP * Math.abs(H[hij(kwtop, kwtop)]))) {
          ns = 0;
          nd = 1;
          if (kwtop > ktop) {
            H[hij(kwtop, kwtop - 1)] = ZERO;
          }
        }
        WORK[offsetWORK] = ONE;
        return { "ns": ns, "nd": nd };
      }
      dlacpy("upper", jw, jw, H, strideH1, strideH2, hij(kwtop, kwtop), T, strideT1, strideT2, offsetT);
      dcopy(jw - 1, H, strideH1 + strideH2, hij(kwtop + 1, kwtop), T, strideT1 + strideT2, tij(2, 1));
      dlaset("full", jw, jw, ZERO, ONE, V, strideV1, strideV2, offsetV);
      if (dlaqr4fn) {
        nmin = 12;
        if (jw > nmin) {
          infqr = dlaqr4fn(true, true, jw, 1, jw, T, strideT1, strideT2, offsetT, SR, strideSR, offsetSR + (kwtop - 1) * strideSR, SI, strideSI, offsetSI + (kwtop - 1) * strideSI, 1, jw, V, strideV1, strideV2, offsetV, WORK, strideWORK, offsetWORK, lwork);
        } else {
          infqr = dlahqr(true, true, jw, 1, jw, T, strideT1, strideT2, offsetT, SR, strideSR, offsetSR + (kwtop - 1) * strideSR, SI, strideSI, offsetSI + (kwtop - 1) * strideSI, 1, jw, V, strideV1, strideV2, offsetV);
        }
      } else {
        infqr = dlahqr(true, true, jw, 1, jw, T, strideT1, strideT2, offsetT, SR, strideSR, offsetSR + (kwtop - 1) * strideSR, SI, strideSI, offsetSI + (kwtop - 1) * strideSI, 1, jw, V, strideV1, strideV2, offsetV);
      }
      for (j = 1; j <= jw - 3; j++) {
        T[tij(j + 2, j)] = ZERO;
        T[tij(j + 3, j)] = ZERO;
      }
      if (jw > 2) {
        T[tij(jw, jw - 2)] = ZERO;
      }
      ns = jw;
      ilst = infqr + 1;
      while (ilst <= ns) {
        if (ns === 1) {
          bulge = false;
        } else {
          bulge = T[tij(ns, ns - 1)] !== ZERO;
        }
        if (!bulge) {
          foo = Math.abs(T[tij(ns, ns)]);
          if (foo === ZERO) {
            foo = Math.abs(s);
          }
          if (Math.abs(s * V[vij(1, ns)]) <= Math.max(smlnum, ULP * foo)) {
            ns = ns - 1;
          } else {
            ifst = ns;
            trxResult = dtrexc("update", jw, T, strideT1, strideT2, offsetT, V, strideV1, strideV2, offsetV, ifst, ilst, WORK, strideWORK, offsetWORK);
            ilst = trxResult.ilst + 1;
          }
        } else {
          foo = Math.abs(T[tij(ns, ns)]) + Math.sqrt(Math.abs(T[tij(ns, ns - 1)])) * Math.sqrt(Math.abs(T[tij(ns - 1, ns)]));
          if (foo === ZERO) {
            foo = Math.abs(s);
          }
          if (Math.max(Math.abs(s * V[vij(1, ns)]), Math.abs(s * V[vij(1, ns - 1)])) <= Math.max(smlnum, ULP * foo)) {
            ns = ns - 2;
          } else {
            ifst = ns;
            trxResult = dtrexc("update", jw, T, strideT1, strideT2, offsetT, V, strideV1, strideV2, offsetV, ifst, ilst, WORK, strideWORK, offsetWORK);
            ilst = trxResult.ilst + 2;
          }
        }
      }
      if (ns === 0) {
        s = ZERO;
      }
      if (ns < jw) {
        sorted = false;
        i = ns + 1;
        while (!sorted) {
          sorted = true;
          kend = i - 1;
          i = infqr + 1;
          if (i === ns) {
            k = i + 1;
          } else if (T[tij(i + 1, i)] === ZERO) {
            k = i + 1;
          } else {
            k = i + 2;
          }
          while (k <= kend) {
            if (k === i + 1) {
              evi = Math.abs(T[tij(i, i)]);
            } else {
              evi = Math.abs(T[tij(i, i)]) + Math.sqrt(Math.abs(T[tij(i + 1, i)])) * Math.sqrt(Math.abs(T[tij(i, i + 1)]));
            }
            if (k === kend) {
              evk = Math.abs(T[tij(k, k)]);
            } else if (T[tij(k + 1, k)] === ZERO) {
              evk = Math.abs(T[tij(k, k)]);
            } else {
              evk = Math.abs(T[tij(k, k)]) + Math.sqrt(Math.abs(T[tij(k + 1, k)])) * Math.sqrt(Math.abs(T[tij(k, k + 1)]));
            }
            if (evi >= evk) {
              i = k;
            } else {
              sorted = false;
              ifst = i;
              trxResult = dtrexc("update", jw, T, strideT1, strideT2, offsetT, V, strideV1, strideV2, offsetV, ifst, k, WORK, strideWORK, offsetWORK);
              if (trxResult.info === 0) {
                i = trxResult.ilst;
              } else {
                i = k;
              }
            }
            if (i === kend) {
              k = i + 1;
            } else if (T[tij(i + 1, i)] === ZERO) {
              k = i + 1;
            } else {
              k = i + 2;
            }
          }
        }
      }
      i = jw;
      while (i >= infqr + 1) {
        if (i === infqr + 1) {
          SR[offsetSR + (kwtop + i - 2) * strideSR] = T[tij(i, i)];
          SI[offsetSI + (kwtop + i - 2) * strideSI] = ZERO;
          i = i - 1;
        } else if (T[tij(i, i - 1)] === ZERO) {
          SR[offsetSR + (kwtop + i - 2) * strideSR] = T[tij(i, i)];
          SI[offsetSI + (kwtop + i - 2) * strideSI] = ZERO;
          i = i - 1;
        } else {
          aa = T[tij(i - 1, i - 1)];
          cc = T[tij(i, i - 1)];
          bb = T[tij(i - 1, i)];
          dd = T[tij(i, i)];
          lv2 = dlanv2(aa, bb, cc, dd);
          SR[offsetSR + (kwtop + i - 3) * strideSR] = lv2.rt1r;
          SI[offsetSI + (kwtop + i - 3) * strideSI] = lv2.rt1i;
          SR[offsetSR + (kwtop + i - 2) * strideSR] = lv2.rt2r;
          SI[offsetSI + (kwtop + i - 2) * strideSI] = lv2.rt2i;
          i = i - 2;
        }
      }
      if (ns < jw || s === ZERO) {
        if (ns > 1 && s !== ZERO) {
          dcopy(ns, V, strideV1, vij(1, 1), WORK, strideWORK, offsetWORK);
          tauArr = new Float64Array(1);
          dlarfg(ns, WORK, offsetWORK, WORK, strideWORK, offsetWORK + strideWORK, tauArr, 0);
          tau = tauArr[0];
          WORK[offsetWORK] = ONE;
          dlaset("lower", jw - 2, jw - 2, ZERO, ZERO, T, strideT1, strideT2, tij(3, 1));
          dlarf("left", ns, jw, WORK, strideWORK, offsetWORK, tau, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK + jw * strideWORK);
          dlarf("right", ns, ns, WORK, strideWORK, offsetWORK, tau, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK + jw * strideWORK);
          dlarf("right", jw, ns, WORK, strideWORK, offsetWORK, tau, V, strideV1, strideV2, offsetV, WORK, strideWORK, offsetWORK + jw * strideWORK);
          dgehrd(jw, 1, ns, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK, WORK, strideWORK, offsetWORK + jw * strideWORK, lwork - jw);
        }
        if (kwtop > 1) {
          H[hij(kwtop, kwtop - 1)] = s * V[vij(1, 1)];
        }
        dlacpy("upper", jw, jw, T, strideT1, strideT2, offsetT, H, strideH1, strideH2, hij(kwtop, kwtop));
        dcopy(jw - 1, T, strideT1 + strideT2, tij(2, 1), H, strideH1 + strideH2, hij(kwtop + 1, kwtop));
        if (ns > 1 && s !== ZERO) {
          dormhr("right", "no-transpose", jw, ns, 1, ns, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK, V, strideV1, strideV2, offsetV, WORK, strideWORK, offsetWORK + jw * strideWORK, lwork - jw);
        }
        if (wantt) {
          ltop = 1;
        } else {
          ltop = ktop;
        }
        for (krow = ltop; krow <= kwtop - 1; krow += nv) {
          kln = Math.min(nv, kwtop - krow);
          dgemm2("no-transpose", "no-transpose", kln, jw, jw, ONE, H, strideH1, strideH2, hij(krow, kwtop), V, strideV1, strideV2, offsetV, ZERO, WV, strideWV1, strideWV2, offsetWV);
          dlacpy("full", kln, jw, WV, strideWV1, strideWV2, offsetWV, H, strideH1, strideH2, hij(krow, kwtop));
        }
        if (wantt) {
          for (kcol = kbot + 1; kcol <= N; kcol += nh) {
            kln = Math.min(nh, N - kcol + 1);
            dgemm2("transpose", "no-transpose", jw, kln, jw, ONE, V, strideV1, strideV2, offsetV, H, strideH1, strideH2, hij(kwtop, kcol), ZERO, T, strideT1, strideT2, offsetT);
            dlacpy("full", jw, kln, T, strideT1, strideT2, offsetT, H, strideH1, strideH2, hij(kwtop, kcol));
          }
        }
        if (wantz) {
          for (krow = iloz; krow <= ihiz; krow += nv) {
            kln = Math.min(nv, ihiz - krow + 1);
            dgemm2("no-transpose", "no-transpose", kln, jw, jw, ONE, Z, strideZ1, strideZ2, zij(krow, kwtop), V, strideV1, strideV2, offsetV, ZERO, WV, strideWV1, strideWV2, offsetWV);
            dlacpy("full", kln, jw, WV, strideWV1, strideWV2, offsetWV, Z, strideZ1, strideZ2, zij(krow, kwtop));
          }
        }
      }
      nd = jw - ns;
      ns = ns - infqr;
      WORK[offsetWORK] = lwkopt;
      return { "ns": ns, "nd": nd };
    }
    module.exports = dlaqr2;
    module.exports.dlaqr23impl = dlaqr23impl;
  }
});

// lib/lapack/base/dlaqr1/lib/base.js
var require_base45 = __commonJS({
  "lib/lapack/base/dlaqr1/lib/base.js"(exports, module) {
    "use strict";
    var ZERO = 0;
    function dlaqr1(N, H, strideH1, strideH2, offsetH, sr1, si1, sr2, si2, v, strideV, offsetV) {
      var h21s;
      var h31s;
      var s;
      if (N !== 2 && N !== 3) {
        return;
      }
      if (N === 2) {
        s = Math.abs(H[offsetH] - sr2) + Math.abs(si2) + Math.abs(H[offsetH + strideH1]);
        if (s === ZERO) {
          v[offsetV] = ZERO;
          v[offsetV + strideV] = ZERO;
        } else {
          h21s = H[offsetH + strideH1] / s;
          v[offsetV] = h21s * H[offsetH + strideH2] + (H[offsetH] - sr1) * ((H[offsetH] - sr2) / s) - si1 * (si2 / s);
          v[offsetV + strideV] = h21s * (H[offsetH] + H[offsetH + strideH1 + strideH2] - sr1 - sr2);
        }
      } else {
        s = Math.abs(H[offsetH] - sr2) + Math.abs(si2) + Math.abs(H[offsetH + strideH1]) + Math.abs(H[offsetH + 2 * strideH1]);
        if (s === ZERO) {
          v[offsetV] = ZERO;
          v[offsetV + strideV] = ZERO;
          v[offsetV + 2 * strideV] = ZERO;
        } else {
          h21s = H[offsetH + strideH1] / s;
          h31s = H[offsetH + 2 * strideH1] / s;
          v[offsetV] = (H[offsetH] - sr1) * ((H[offsetH] - sr2) / s) - si1 * (si2 / s) + H[offsetH + strideH2] * h21s + H[offsetH + 2 * strideH2] * h31s;
          v[offsetV + strideV] = h21s * (H[offsetH] + H[offsetH + strideH1 + strideH2] - sr1 - sr2) + H[offsetH + strideH1 + 2 * strideH2] * h31s;
          v[offsetV + 2 * strideV] = h31s * (H[offsetH] + H[offsetH + 2 * strideH1 + 2 * strideH2] - sr1 - sr2) + h21s * H[offsetH + 2 * strideH1 + strideH2];
        }
      }
    }
    module.exports = dlaqr1;
  }
});

// lib/lapack/base/dlaqr5/lib/base.js
var require_base46 = __commonJS({
  "lib/lapack/base/dlaqr5/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlaqr1 = require_base45();
    var dlarfg = require_base20();
    var dlaset = require_base35();
    var dlacpy = require_base9();
    var dgemm2 = require_base22();
    var SAFMIN = dlamch("safe-minimum");
    var ULP = dlamch("epsilon");
    function get2d(A, sA1, sA2, oA, i, j) {
      return A[oA + (i - 1) * sA1 + (j - 1) * sA2];
    }
    function set2d(A, sA1, sA2, oA, i, j, val) {
      A[oA + (i - 1) * sA1 + (j - 1) * sA2] = val;
    }
    function idx2d(sA1, sA2, oA, i, j) {
      return oA + (i - 1) * sA1 + (j - 1) * sA2;
    }
    function dlaqr5(wantt, wantz, kacc22, N, ktop, kbot, nshfts, SR, strideSR, offsetSR, SI, strideSI, offsetSI, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, V, strideV1, strideV2, offsetV, U, strideU1, strideU2, offsetU, nv, WV, strideWV1, strideWV2, offsetWV, nh, WH, strideWH1, strideWH2, offsetWH) {
      var smlnum;
      var refsum;
      var safmax;
      var safmin;
      var nbmps;
      var ndcol;
      var krcol;
      var accum;
      var bmp22;
      var alpha;
      var incol;
      var swap;
      var beta;
      var jtop;
      var jbot;
      var jcol;
      var jlen;
      var jrow;
      var mbot;
      var mtop;
      var tst1;
      var tst2;
      var h11;
      var h12;
      var h21;
      var h22;
      var scl;
      var kdu;
      var kms;
      var m22;
      var vt;
      var ns;
      var nu;
      var k1;
      var i2;
      var i4;
      var t1;
      var t2;
      var t3;
      var k;
      var m;
      var j;
      var i;
      var KTOP = ktop + 1;
      var KBOT = kbot + 1;
      var ILOZ = iloz + 1;
      var IHIZ = ihiz + 1;
      var sH1 = strideH1;
      var sH2 = strideH2;
      var oH = offsetH;
      var sZ1 = strideZ1;
      var sZ2 = strideZ2;
      var oZ = offsetZ;
      var sV1 = strideV1;
      var sV2 = strideV2;
      var oV = offsetV;
      var sU1 = strideU1;
      var sU2 = strideU2;
      var oU = offsetU;
      var sWV1 = strideWV1;
      var sWV2 = strideWV2;
      var oWV = offsetWV;
      var sWH1 = strideWH1;
      var sWH2 = strideWH2;
      var oWH = offsetWH;
      vt = new Float64Array(3);
      var alphaArr = new Float64Array(1);
      var tauArr = new Float64Array(1);
      if (nshfts < 2) {
        return;
      }
      if (KTOP >= KBOT) {
        return;
      }
      for (i = 1; i <= nshfts - 2; i += 2) {
        if (SI[offsetSI + (i - 1) * strideSI] !== -SI[offsetSI + i * strideSI]) {
          swap = SR[offsetSR + (i - 1) * strideSR];
          SR[offsetSR + (i - 1) * strideSR] = SR[offsetSR + i * strideSR];
          SR[offsetSR + i * strideSR] = SR[offsetSR + (i + 1) * strideSR];
          SR[offsetSR + (i + 1) * strideSR] = swap;
          swap = SI[offsetSI + (i - 1) * strideSI];
          SI[offsetSI + (i - 1) * strideSI] = SI[offsetSI + i * strideSI];
          SI[offsetSI + i * strideSI] = SI[offsetSI + (i + 1) * strideSI];
          SI[offsetSI + (i + 1) * strideSI] = swap;
        }
      }
      ns = nshfts - nshfts % 2;
      safmin = SAFMIN;
      safmax = 1 / safmin;
      smlnum = safmin * (N / ULP);
      accum = kacc22 === 1 || kacc22 === 2;
      if (KTOP + 2 <= KBOT) {
        set2d(H, sH1, sH2, oH, KTOP + 2, KTOP, 0);
      }
      nbmps = ns / 2 | 0;
      kdu = 4 * nbmps;
      function sr(idx) {
        return SR[offsetSR + (idx - 1) * strideSR];
      }
      function si(idx) {
        return SI[offsetSI + (idx - 1) * strideSI];
      }
      for (incol = KTOP - 2 * nbmps + 1; incol <= KBOT - 2; incol += 2 * nbmps) {
        if (accum) {
          jtop = Math.max(KTOP, incol);
        } else if (wantt) {
          jtop = 1;
        } else {
          jtop = KTOP;
        }
        ndcol = incol + kdu;
        if (accum) {
          dlaset("ALL", kdu, kdu, 0, 1, U, sU1, sU2, oU);
        }
        for (krcol = incol; krcol <= Math.min(incol + 2 * nbmps - 1, KBOT - 2); krcol++) {
          mtop = Math.max(1, (KTOP - krcol) / 2 + 1 | 0);
          mbot = Math.min(nbmps, (KBOT - krcol - 1) / 2 | 0);
          m22 = mbot + 1;
          bmp22 = mbot < nbmps && krcol + 2 * (m22 - 1) === KBOT - 2;
          if (bmp22) {
            k = krcol + 2 * (m22 - 1);
            if (k === KTOP - 1) {
              dlaqr1(
                2,
                H,
                sH1,
                sH2,
                idx2d(sH1, sH2, oH, k + 1, k + 1),
                sr(2 * m22 - 1),
                si(2 * m22 - 1),
                sr(2 * m22),
                si(2 * m22),
                V,
                sV1,
                idx2d(sV1, sV2, oV, 1, m22)
              );
              alphaArr[0] = get2d(V, sV1, sV2, oV, 1, m22);
              dlarfg(2, alphaArr, 0, V, sV1, idx2d(sV1, sV2, oV, 2, m22), tauArr, 0);
              set2d(V, sV1, sV2, oV, 1, m22, tauArr[0]);
            } else {
              alphaArr[0] = get2d(H, sH1, sH2, oH, k + 1, k);
              set2d(V, sV1, sV2, oV, 2, m22, get2d(H, sH1, sH2, oH, k + 2, k));
              dlarfg(2, alphaArr, 0, V, sV1, idx2d(sV1, sV2, oV, 2, m22), tauArr, 0);
              set2d(V, sV1, sV2, oV, 1, m22, tauArr[0]);
              set2d(H, sH1, sH2, oH, k + 1, k, alphaArr[0]);
              set2d(H, sH1, sH2, oH, k + 2, k, 0);
            }
            t1 = get2d(V, sV1, sV2, oV, 1, m22);
            t2 = t1 * get2d(V, sV1, sV2, oV, 2, m22);
            for (j = jtop; j <= Math.min(KBOT, k + 3); j++) {
              refsum = get2d(H, sH1, sH2, oH, j, k + 1) + get2d(V, sV1, sV2, oV, 2, m22) * get2d(H, sH1, sH2, oH, j, k + 2);
              set2d(H, sH1, sH2, oH, j, k + 1, get2d(H, sH1, sH2, oH, j, k + 1) - refsum * t1);
              set2d(H, sH1, sH2, oH, j, k + 2, get2d(H, sH1, sH2, oH, j, k + 2) - refsum * t2);
            }
            if (accum) {
              jbot = Math.min(ndcol, KBOT);
            } else if (wantt) {
              jbot = N;
            } else {
              jbot = KBOT;
            }
            t1 = get2d(V, sV1, sV2, oV, 1, m22);
            t2 = t1 * get2d(V, sV1, sV2, oV, 2, m22);
            for (j = k + 1; j <= jbot; j++) {
              refsum = get2d(H, sH1, sH2, oH, k + 1, j) + get2d(V, sV1, sV2, oV, 2, m22) * get2d(H, sH1, sH2, oH, k + 2, j);
              set2d(H, sH1, sH2, oH, k + 1, j, get2d(H, sH1, sH2, oH, k + 1, j) - refsum * t1);
              set2d(H, sH1, sH2, oH, k + 2, j, get2d(H, sH1, sH2, oH, k + 2, j) - refsum * t2);
            }
            if (k >= KTOP) {
              if (get2d(H, sH1, sH2, oH, k + 1, k) !== 0) {
                tst1 = Math.abs(get2d(H, sH1, sH2, oH, k, k)) + Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1));
                if (tst1 === 0) {
                  if (k >= KTOP + 1) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 1));
                  }
                  if (k >= KTOP + 2) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 2));
                  }
                  if (k >= KTOP + 3) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 3));
                  }
                  if (k <= KBOT - 2) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 2, k + 1));
                  }
                  if (k <= KBOT - 3) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 3, k + 1));
                  }
                  if (k <= KBOT - 4) {
                    tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 4, k + 1));
                  }
                }
                if (Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)) <= Math.max(smlnum, ULP * tst1)) {
                  h12 = Math.max(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)), Math.abs(get2d(H, sH1, sH2, oH, k, k + 1)));
                  h21 = Math.min(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)), Math.abs(get2d(H, sH1, sH2, oH, k, k + 1)));
                  h11 = Math.max(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1)), Math.abs(get2d(H, sH1, sH2, oH, k, k) - get2d(H, sH1, sH2, oH, k + 1, k + 1)));
                  h22 = Math.min(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1)), Math.abs(get2d(H, sH1, sH2, oH, k, k) - get2d(H, sH1, sH2, oH, k + 1, k + 1)));
                  scl = h11 + h12;
                  tst2 = h22 * (h11 / scl);
                  if (tst2 === 0 || h21 * (h12 / scl) <= Math.max(smlnum, ULP * tst2)) {
                    set2d(H, sH1, sH2, oH, k + 1, k, 0);
                  }
                }
              }
            }
            if (accum) {
              kms = k - incol;
              t1 = get2d(V, sV1, sV2, oV, 1, m22);
              t2 = t1 * get2d(V, sV1, sV2, oV, 2, m22);
              for (j = Math.max(1, KTOP - incol); j <= kdu; j++) {
                refsum = get2d(U, sU1, sU2, oU, j, kms + 1) + get2d(V, sV1, sV2, oV, 2, m22) * get2d(U, sU1, sU2, oU, j, kms + 2);
                set2d(U, sU1, sU2, oU, j, kms + 1, get2d(U, sU1, sU2, oU, j, kms + 1) - refsum * t1);
                set2d(U, sU1, sU2, oU, j, kms + 2, get2d(U, sU1, sU2, oU, j, kms + 2) - refsum * t2);
              }
            } else if (wantz) {
              t1 = get2d(V, sV1, sV2, oV, 1, m22);
              t2 = t1 * get2d(V, sV1, sV2, oV, 2, m22);
              for (j = ILOZ; j <= IHIZ; j++) {
                refsum = get2d(Z, sZ1, sZ2, oZ, j, k + 1) + get2d(V, sV1, sV2, oV, 2, m22) * get2d(Z, sZ1, sZ2, oZ, j, k + 2);
                set2d(Z, sZ1, sZ2, oZ, j, k + 1, get2d(Z, sZ1, sZ2, oZ, j, k + 1) - refsum * t1);
                set2d(Z, sZ1, sZ2, oZ, j, k + 2, get2d(Z, sZ1, sZ2, oZ, j, k + 2) - refsum * t2);
              }
            }
          }
          for (m = mbot; m >= mtop; m--) {
            k = krcol + 2 * (m - 1);
            if (k === KTOP - 1) {
              dlaqr1(
                3,
                H,
                sH1,
                sH2,
                idx2d(sH1, sH2, oH, KTOP, KTOP),
                sr(2 * m - 1),
                si(2 * m - 1),
                sr(2 * m),
                si(2 * m),
                V,
                sV1,
                idx2d(sV1, sV2, oV, 1, m)
              );
              alphaArr[0] = get2d(V, sV1, sV2, oV, 1, m);
              dlarfg(3, alphaArr, 0, V, sV1, idx2d(sV1, sV2, oV, 2, m), tauArr, 0);
              set2d(V, sV1, sV2, oV, 1, m, tauArr[0]);
            } else {
              t1 = get2d(V, sV1, sV2, oV, 1, m);
              t2 = t1 * get2d(V, sV1, sV2, oV, 2, m);
              t3 = t1 * get2d(V, sV1, sV2, oV, 3, m);
              refsum = get2d(V, sV1, sV2, oV, 3, m) * get2d(H, sH1, sH2, oH, k + 3, k + 2);
              set2d(H, sH1, sH2, oH, k + 3, k, -refsum * t1);
              set2d(H, sH1, sH2, oH, k + 3, k + 1, -refsum * t2);
              set2d(H, sH1, sH2, oH, k + 3, k + 2, get2d(H, sH1, sH2, oH, k + 3, k + 2) - refsum * t3);
              alphaArr[0] = get2d(H, sH1, sH2, oH, k + 1, k);
              set2d(V, sV1, sV2, oV, 2, m, get2d(H, sH1, sH2, oH, k + 2, k));
              set2d(V, sV1, sV2, oV, 3, m, get2d(H, sH1, sH2, oH, k + 3, k));
              dlarfg(3, alphaArr, 0, V, sV1, idx2d(sV1, sV2, oV, 2, m), tauArr, 0);
              beta = alphaArr[0];
              set2d(V, sV1, sV2, oV, 1, m, tauArr[0]);
              if (get2d(H, sH1, sH2, oH, k + 3, k) !== 0 || get2d(H, sH1, sH2, oH, k + 3, k + 1) !== 0 || get2d(H, sH1, sH2, oH, k + 3, k + 2) === 0) {
                set2d(H, sH1, sH2, oH, k + 1, k, beta);
                set2d(H, sH1, sH2, oH, k + 2, k, 0);
                set2d(H, sH1, sH2, oH, k + 3, k, 0);
              } else {
                dlaqr1(
                  3,
                  H,
                  sH1,
                  sH2,
                  idx2d(sH1, sH2, oH, k + 1, k + 1),
                  sr(2 * m - 1),
                  si(2 * m - 1),
                  sr(2 * m),
                  si(2 * m),
                  vt,
                  1,
                  0
                );
                alphaArr[0] = vt[0];
                dlarfg(3, alphaArr, 0, vt, 1, 1, tauArr, 0);
                vt[0] = tauArr[0];
                t1 = vt[0];
                t2 = t1 * vt[1];
                t3 = t1 * vt[2];
                refsum = get2d(H, sH1, sH2, oH, k + 1, k) + vt[1] * get2d(H, sH1, sH2, oH, k + 2, k);
                if (Math.abs(get2d(H, sH1, sH2, oH, k + 2, k) - refsum * t2) + Math.abs(refsum * t3) > ULP * (Math.abs(get2d(H, sH1, sH2, oH, k, k)) + Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1)) + Math.abs(get2d(H, sH1, sH2, oH, k + 2, k + 2)))) {
                  set2d(H, sH1, sH2, oH, k + 1, k, beta);
                  set2d(H, sH1, sH2, oH, k + 2, k, 0);
                  set2d(H, sH1, sH2, oH, k + 3, k, 0);
                } else {
                  set2d(H, sH1, sH2, oH, k + 1, k, get2d(H, sH1, sH2, oH, k + 1, k) - refsum * t1);
                  set2d(H, sH1, sH2, oH, k + 2, k, 0);
                  set2d(H, sH1, sH2, oH, k + 3, k, 0);
                  set2d(V, sV1, sV2, oV, 1, m, vt[0]);
                  set2d(V, sV1, sV2, oV, 2, m, vt[1]);
                  set2d(V, sV1, sV2, oV, 3, m, vt[2]);
                }
              }
            }
            t1 = get2d(V, sV1, sV2, oV, 1, m);
            t2 = t1 * get2d(V, sV1, sV2, oV, 2, m);
            t3 = t1 * get2d(V, sV1, sV2, oV, 3, m);
            for (j = jtop; j <= Math.min(KBOT, k + 3); j++) {
              refsum = get2d(H, sH1, sH2, oH, j, k + 1) + get2d(V, sV1, sV2, oV, 2, m) * get2d(H, sH1, sH2, oH, j, k + 2) + get2d(V, sV1, sV2, oV, 3, m) * get2d(H, sH1, sH2, oH, j, k + 3);
              set2d(H, sH1, sH2, oH, j, k + 1, get2d(H, sH1, sH2, oH, j, k + 1) - refsum * t1);
              set2d(H, sH1, sH2, oH, j, k + 2, get2d(H, sH1, sH2, oH, j, k + 2) - refsum * t2);
              set2d(H, sH1, sH2, oH, j, k + 3, get2d(H, sH1, sH2, oH, j, k + 3) - refsum * t3);
            }
            refsum = get2d(H, sH1, sH2, oH, k + 1, k + 1) + get2d(V, sV1, sV2, oV, 2, m) * get2d(H, sH1, sH2, oH, k + 2, k + 1) + get2d(V, sV1, sV2, oV, 3, m) * get2d(H, sH1, sH2, oH, k + 3, k + 1);
            set2d(H, sH1, sH2, oH, k + 1, k + 1, get2d(H, sH1, sH2, oH, k + 1, k + 1) - refsum * t1);
            set2d(H, sH1, sH2, oH, k + 2, k + 1, get2d(H, sH1, sH2, oH, k + 2, k + 1) - refsum * t2);
            set2d(H, sH1, sH2, oH, k + 3, k + 1, get2d(H, sH1, sH2, oH, k + 3, k + 1) - refsum * t3);
            if (k < KTOP) {
              continue;
            }
            if (get2d(H, sH1, sH2, oH, k + 1, k) !== 0) {
              tst1 = Math.abs(get2d(H, sH1, sH2, oH, k, k)) + Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1));
              if (tst1 === 0) {
                if (k >= KTOP + 1) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 1));
                }
                if (k >= KTOP + 2) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 2));
                }
                if (k >= KTOP + 3) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k, k - 3));
                }
                if (k <= KBOT - 2) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 2, k + 1));
                }
                if (k <= KBOT - 3) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 3, k + 1));
                }
                if (k <= KBOT - 4) {
                  tst1 = tst1 + Math.abs(get2d(H, sH1, sH2, oH, k + 4, k + 1));
                }
              }
              if (Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)) <= Math.max(smlnum, ULP * tst1)) {
                h12 = Math.max(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)), Math.abs(get2d(H, sH1, sH2, oH, k, k + 1)));
                h21 = Math.min(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k)), Math.abs(get2d(H, sH1, sH2, oH, k, k + 1)));
                h11 = Math.max(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1)), Math.abs(get2d(H, sH1, sH2, oH, k, k) - get2d(H, sH1, sH2, oH, k + 1, k + 1)));
                h22 = Math.min(Math.abs(get2d(H, sH1, sH2, oH, k + 1, k + 1)), Math.abs(get2d(H, sH1, sH2, oH, k, k) - get2d(H, sH1, sH2, oH, k + 1, k + 1)));
                scl = h11 + h12;
                tst2 = h22 * (h11 / scl);
                if (tst2 === 0 || h21 * (h12 / scl) <= Math.max(smlnum, ULP * tst2)) {
                  set2d(H, sH1, sH2, oH, k + 1, k, 0);
                }
              }
            }
          }
          if (accum) {
            jbot = Math.min(ndcol, KBOT);
          } else if (wantt) {
            jbot = N;
          } else {
            jbot = KBOT;
          }
          for (m = mbot; m >= mtop; m--) {
            k = krcol + 2 * (m - 1);
            t1 = get2d(V, sV1, sV2, oV, 1, m);
            t2 = t1 * get2d(V, sV1, sV2, oV, 2, m);
            t3 = t1 * get2d(V, sV1, sV2, oV, 3, m);
            for (j = Math.max(KTOP, krcol + 2 * m); j <= jbot; j++) {
              refsum = get2d(H, sH1, sH2, oH, k + 1, j) + get2d(V, sV1, sV2, oV, 2, m) * get2d(H, sH1, sH2, oH, k + 2, j) + get2d(V, sV1, sV2, oV, 3, m) * get2d(H, sH1, sH2, oH, k + 3, j);
              set2d(H, sH1, sH2, oH, k + 1, j, get2d(H, sH1, sH2, oH, k + 1, j) - refsum * t1);
              set2d(H, sH1, sH2, oH, k + 2, j, get2d(H, sH1, sH2, oH, k + 2, j) - refsum * t2);
              set2d(H, sH1, sH2, oH, k + 3, j, get2d(H, sH1, sH2, oH, k + 3, j) - refsum * t3);
            }
          }
          if (accum) {
            for (m = mbot; m >= mtop; m--) {
              k = krcol + 2 * (m - 1);
              kms = k - incol;
              i2 = Math.max(1, KTOP - incol);
              i2 = Math.max(i2, kms - (krcol - incol) + 1);
              i4 = Math.min(kdu, krcol + 2 * (mbot - 1) - incol + 5);
              t1 = get2d(V, sV1, sV2, oV, 1, m);
              t2 = t1 * get2d(V, sV1, sV2, oV, 2, m);
              t3 = t1 * get2d(V, sV1, sV2, oV, 3, m);
              for (j = i2; j <= i4; j++) {
                refsum = get2d(U, sU1, sU2, oU, j, kms + 1) + get2d(V, sV1, sV2, oV, 2, m) * get2d(U, sU1, sU2, oU, j, kms + 2) + get2d(V, sV1, sV2, oV, 3, m) * get2d(U, sU1, sU2, oU, j, kms + 3);
                set2d(U, sU1, sU2, oU, j, kms + 1, get2d(U, sU1, sU2, oU, j, kms + 1) - refsum * t1);
                set2d(U, sU1, sU2, oU, j, kms + 2, get2d(U, sU1, sU2, oU, j, kms + 2) - refsum * t2);
                set2d(U, sU1, sU2, oU, j, kms + 3, get2d(U, sU1, sU2, oU, j, kms + 3) - refsum * t3);
              }
            }
          } else if (wantz) {
            for (m = mbot; m >= mtop; m--) {
              k = krcol + 2 * (m - 1);
              t1 = get2d(V, sV1, sV2, oV, 1, m);
              t2 = t1 * get2d(V, sV1, sV2, oV, 2, m);
              t3 = t1 * get2d(V, sV1, sV2, oV, 3, m);
              for (j = ILOZ; j <= IHIZ; j++) {
                refsum = get2d(Z, sZ1, sZ2, oZ, j, k + 1) + get2d(V, sV1, sV2, oV, 2, m) * get2d(Z, sZ1, sZ2, oZ, j, k + 2) + get2d(V, sV1, sV2, oV, 3, m) * get2d(Z, sZ1, sZ2, oZ, j, k + 3);
                set2d(Z, sZ1, sZ2, oZ, j, k + 1, get2d(Z, sZ1, sZ2, oZ, j, k + 1) - refsum * t1);
                set2d(Z, sZ1, sZ2, oZ, j, k + 2, get2d(Z, sZ1, sZ2, oZ, j, k + 2) - refsum * t2);
                set2d(Z, sZ1, sZ2, oZ, j, k + 3, get2d(Z, sZ1, sZ2, oZ, j, k + 3) - refsum * t3);
              }
            }
          }
        }
        if (accum) {
          if (wantt) {
            jtop = 1;
            jbot = N;
          } else {
            jtop = KTOP;
            jbot = KBOT;
          }
          k1 = Math.max(1, KTOP - incol);
          nu = kdu - Math.max(0, ndcol - KBOT) - k1 + 1;
          for (jcol = Math.min(ndcol, KBOT) + 1; jcol <= jbot; jcol += nh) {
            jlen = Math.min(nh, jbot - jcol + 1);
            dgemm2(
              "transpose",
              "no-transpose",
              nu,
              jlen,
              nu,
              1,
              U,
              sU1,
              sU2,
              idx2d(sU1, sU2, oU, k1, k1),
              H,
              sH1,
              sH2,
              idx2d(sH1, sH2, oH, incol + k1, jcol),
              0,
              WH,
              sWH1,
              sWH2,
              oWH
            );
            dlacpy(
              "ALL",
              nu,
              jlen,
              WH,
              sWH1,
              sWH2,
              oWH,
              H,
              sH1,
              sH2,
              idx2d(sH1, sH2, oH, incol + k1, jcol)
            );
          }
          for (jrow = jtop; jrow <= Math.max(KTOP, incol) - 1; jrow += nv) {
            jlen = Math.min(nv, Math.max(KTOP, incol) - jrow);
            dgemm2(
              "no-transpose",
              "no-transpose",
              jlen,
              nu,
              nu,
              1,
              H,
              sH1,
              sH2,
              idx2d(sH1, sH2, oH, jrow, incol + k1),
              U,
              sU1,
              sU2,
              idx2d(sU1, sU2, oU, k1, k1),
              0,
              WV,
              sWV1,
              sWV2,
              oWV
            );
            dlacpy(
              "ALL",
              jlen,
              nu,
              WV,
              sWV1,
              sWV2,
              oWV,
              H,
              sH1,
              sH2,
              idx2d(sH1, sH2, oH, jrow, incol + k1)
            );
          }
          if (wantz) {
            for (jrow = ILOZ; jrow <= IHIZ; jrow += nv) {
              jlen = Math.min(nv, IHIZ - jrow + 1);
              dgemm2(
                "no-transpose",
                "no-transpose",
                jlen,
                nu,
                nu,
                1,
                Z,
                sZ1,
                sZ2,
                idx2d(sZ1, sZ2, oZ, jrow, incol + k1),
                U,
                sU1,
                sU2,
                idx2d(sU1, sU2, oU, k1, k1),
                0,
                WV,
                sWV1,
                sWV2,
                oWV
              );
              dlacpy(
                "ALL",
                jlen,
                nu,
                WV,
                sWV1,
                sWV2,
                oWV,
                Z,
                sZ1,
                sZ2,
                idx2d(sZ1, sZ2, oZ, jrow, incol + k1)
              );
            }
          }
        }
      }
    }
    module.exports = dlaqr5;
  }
});

// lib/lapack/base/dlaqr4/lib/base.js
var require_base47 = __commonJS({
  "lib/lapack/base/dlaqr4/lib/base.js"(exports, module) {
    "use strict";
    var dlahqr = require_base34();
    var dlacpy = require_base9();
    var dlanv2 = require_base33();
    var dlaqr2 = require_base44();
    var dlaqr5 = require_base46();
    var ZERO = 0;
    var ONE = 1;
    var NTINY = 15;
    var KEXNW = 5;
    var KEXSH = 6;
    var WILK1 = 0.75;
    var WILK2 = -0.4375;
    function dlaqr4(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork) {
      var lwkopt;
      var nwupbd;
      var nsmax;
      var sorted;
      var itmax;
      var kwtop;
      var kbot;
      var ktop;
      var ndfl;
      var ndec;
      var info;
      var swap;
      var nmin;
      var nwr;
      var nsr;
      var nw;
      var nh;
      var nho;
      var nve;
      var ns;
      var ks;
      var ld;
      var ls;
      var kv;
      var kt;
      var kwv;
      var kwh;
      var ku;
      var kdu;
      var aa;
      var bb;
      var cc;
      var dd;
      var cs;
      var sn;
      var ss;
      var lv2;
      var it;
      var k;
      var i;
      var nibble;
      var kacc22;
      var inf;
      var aedResult;
      var zdum;
      function hij(r, c) {
        return offsetH + (r - 1) * strideH1 + (c - 1) * strideH2;
      }
      info = 0;
      if (N === 0) {
        WORK[offsetWORK] = ONE;
        return 0;
      }
      if (N <= NTINY) {
        lwkopt = 1;
        if (lwork !== -1) {
          info = dlahqr(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ);
        }
      } else {
        info = 0;
        nwr = 4;
        nwr = Math.max(2, nwr);
        nwr = Math.min(ihi - ilo + 1, Math.floor((N - 1) / 3), nwr);
        nsr = 2;
        nsr = Math.min(nsr, Math.floor((N - 3) / 6), ihi - ilo);
        nsr = Math.max(2, nsr - nsr % 2);
        dlaqr2(wantt, wantz, N, ilo, ihi, nwr + 1, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WR, strideWR, offsetWR, WI, strideWI, offsetWI, H, strideH1, strideH2, offsetH, N, H, strideH1, strideH2, offsetH, N, H, strideH1, strideH2, offsetH, WORK, strideWORK, offsetWORK, -1);
        lwkopt = Math.max(Math.floor(3 * nsr / 2), Math.floor(WORK[offsetWORK]));
        if (lwork === -1) {
          WORK[offsetWORK] = lwkopt;
          return 0;
        }
        nmin = NTINY;
        nibble = 14;
        kacc22 = 0;
        kacc22 = Math.max(0, Math.min(2, kacc22));
        var nwmax = Math.min(Math.floor((N - 1) / 3), Math.floor(lwork / 2));
        nw = nwmax;
        nsmax = Math.min(Math.floor((N - 3) / 6), Math.floor(2 * lwork / 3));
        nsmax = nsmax - nsmax % 2;
        ndfl = 1;
        itmax = Math.max(30, 2 * KEXSH) * Math.max(10, ihi - ilo + 1);
        kbot = ihi;
        for (it = 1; it <= itmax; it++) {
          if (kbot < ilo) {
            info = 0;
            break;
          }
          for (k = kbot; k >= ilo + 1; k--) {
            if (H[hij(k, k - 1)] === ZERO) {
              break;
            }
          }
          if (k > ilo && H[hij(k, k - 1)] !== ZERO) {
            k = ilo;
          }
          ktop = k;
          nh = kbot - ktop + 1;
          nwupbd = Math.min(nh, nwmax);
          if (ndfl < KEXNW) {
            nw = Math.min(nwupbd, nwr);
          } else {
            nw = Math.min(nwupbd, 2 * nw);
          }
          if (nw < nwmax) {
            if (nw >= nh - 1) {
              nw = nh;
            } else {
              kwtop = kbot - nw + 1;
              if (Math.abs(H[hij(kwtop, kwtop - 1)]) > Math.abs(H[hij(kwtop - 1, kwtop - 2)])) {
                nw = nw + 1;
              }
            }
          }
          if (ndfl < KEXNW) {
            ndec = -1;
          } else if (ndec >= 0 || nw >= nwupbd) {
            ndec = ndec + 1;
            if (nw - ndec < 2) {
              ndec = 0;
            }
            nw = nw - ndec;
          }
          kv = N - nw + 1;
          kt = nw + 1;
          nho = N - nw - 1 - kt + 1;
          kwv = nw + 2;
          nve = N - nw - kwv + 1;
          aedResult = dlaqr2(wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WR, strideWR, offsetWR, WI, strideWI, offsetWI, H, strideH1, strideH2, hij(kv, 1), nho, H, strideH1, strideH2, hij(kv, kt), nve, H, strideH1, strideH2, hij(kwv, 1), WORK, strideWORK, offsetWORK, lwork);
          ls = aedResult.ns;
          ld = aedResult.nd;
          kbot = kbot - ld;
          ks = kbot - ls + 1;
          if (ld === 0 || 100 * ld <= nw * nibble && kbot - ktop + 1 > Math.min(nmin, nwmax)) {
            ns = Math.min(nsmax, nsr, Math.max(2, kbot - ktop));
            ns = ns - ns % 2;
            if (ndfl % KEXSH === 0) {
              ks = kbot - ns + 1;
              for (i = kbot; i >= Math.max(ks + 1, ktop + 2); i -= 2) {
                ss = Math.abs(H[hij(i, i - 1)]) + Math.abs(H[hij(i - 1, i - 2)]);
                aa = WILK1 * ss + H[hij(i, i)];
                bb = ss;
                cc = WILK2 * ss;
                dd = aa;
                lv2 = dlanv2(aa, bb, cc, dd);
                WR[offsetWR + (i - 2) * strideWR] = lv2.rt1r;
                WI[offsetWI + (i - 2) * strideWI] = lv2.rt1i;
                WR[offsetWR + (i - 1) * strideWR] = lv2.rt2r;
                WI[offsetWI + (i - 1) * strideWI] = lv2.rt2i;
              }
              if (ks === ktop) {
                WR[offsetWR + ks * strideWR] = H[hij(ks + 1, ks + 1)];
                WI[offsetWI + ks * strideWI] = ZERO;
                WR[offsetWR + (ks - 1) * strideWR] = WR[offsetWR + ks * strideWR];
                WI[offsetWI + (ks - 1) * strideWI] = WI[offsetWI + ks * strideWI];
              }
            } else {
              if (kbot - ks + 1 <= Math.floor(ns / 2)) {
                ks = kbot - ns + 1;
                kt = N - ns + 1;
                dlacpy("full", ns, ns, H, strideH1, strideH2, hij(ks, ks), H, strideH1, strideH2, hij(kt, 1));
                zdum = new Float64Array(1);
                inf = dlahqr(false, false, ns, 1, ns, H, strideH1, strideH2, hij(kt, 1), WR, strideWR, offsetWR + (ks - 1) * strideWR, WI, strideWI, offsetWI + (ks - 1) * strideWI, 1, 1, zdum, 1, 1, 0);
                ks = ks + inf;
                if (ks >= kbot) {
                  aa = H[hij(kbot - 1, kbot - 1)];
                  cc = H[hij(kbot, kbot - 1)];
                  bb = H[hij(kbot - 1, kbot)];
                  dd = H[hij(kbot, kbot)];
                  lv2 = dlanv2(aa, bb, cc, dd);
                  WR[offsetWR + (kbot - 2) * strideWR] = lv2.rt1r;
                  WI[offsetWI + (kbot - 2) * strideWI] = lv2.rt1i;
                  WR[offsetWR + (kbot - 1) * strideWR] = lv2.rt2r;
                  WI[offsetWI + (kbot - 1) * strideWI] = lv2.rt2i;
                  ks = kbot - 1;
                }
              }
              if (kbot - ks + 1 > ns) {
                sorted = false;
                for (k = kbot; k >= ks + 1; k--) {
                  if (sorted) {
                    break;
                  }
                  sorted = true;
                  for (i = ks; i <= k - 1; i++) {
                    if (Math.abs(WR[offsetWR + (i - 1) * strideWR]) + Math.abs(WI[offsetWI + (i - 1) * strideWI]) < Math.abs(WR[offsetWR + i * strideWR]) + Math.abs(WI[offsetWI + i * strideWI])) {
                      sorted = false;
                      swap = WR[offsetWR + (i - 1) * strideWR];
                      WR[offsetWR + (i - 1) * strideWR] = WR[offsetWR + i * strideWR];
                      WR[offsetWR + i * strideWR] = swap;
                      swap = WI[offsetWI + (i - 1) * strideWI];
                      WI[offsetWI + (i - 1) * strideWI] = WI[offsetWI + i * strideWI];
                      WI[offsetWI + i * strideWI] = swap;
                    }
                  }
                }
              }
              for (i = kbot; i >= ks + 2; i -= 2) {
                if (WI[offsetWI + (i - 1) * strideWI] !== -WI[offsetWI + (i - 2) * strideWI]) {
                  swap = WR[offsetWR + (i - 1) * strideWR];
                  WR[offsetWR + (i - 1) * strideWR] = WR[offsetWR + (i - 2) * strideWR];
                  WR[offsetWR + (i - 2) * strideWR] = WR[offsetWR + (i - 3) * strideWR];
                  WR[offsetWR + (i - 3) * strideWR] = swap;
                  swap = WI[offsetWI + (i - 1) * strideWI];
                  WI[offsetWI + (i - 1) * strideWI] = WI[offsetWI + (i - 2) * strideWI];
                  WI[offsetWI + (i - 2) * strideWI] = WI[offsetWI + (i - 3) * strideWI];
                  WI[offsetWI + (i - 3) * strideWI] = swap;
                }
              }
            }
            if (kbot - ks + 1 === 2) {
              if (WI[offsetWI + (kbot - 1) * strideWI] === ZERO) {
                if (Math.abs(WR[offsetWR + (kbot - 1) * strideWR] - H[hij(kbot, kbot)]) < Math.abs(WR[offsetWR + (kbot - 2) * strideWR] - H[hij(kbot, kbot)])) {
                  WR[offsetWR + (kbot - 2) * strideWR] = WR[offsetWR + (kbot - 1) * strideWR];
                } else {
                  WR[offsetWR + (kbot - 1) * strideWR] = WR[offsetWR + (kbot - 2) * strideWR];
                }
              }
            }
            ns = Math.min(ns, kbot - ks + 1);
            ns = ns - ns % 2;
            ks = kbot - ns + 1;
            kdu = 2 * ns;
            ku = N - kdu + 1;
            kwh = kdu + 1;
            nho = N - kdu + 1 - 4 - (kdu + 1) + 1;
            kwv = kdu + 4;
            nve = N - kdu - kwv + 1;
            dlaqr5(wantt, wantz, kacc22, N, ktop - 1, kbot - 1, ns, WR, strideWR, offsetWR + (ks - 1) * strideWR, WI, strideWI, offsetWI + (ks - 1) * strideWI, H, strideH1, strideH2, offsetH, iloz - 1, ihiz - 1, Z, strideZ1, strideZ2, offsetZ, WORK, 1, 3, offsetWORK, H, strideH1, strideH2, hij(ku, 1), nve, H, strideH1, strideH2, hij(kwv, 1), nho, H, strideH1, strideH2, hij(ku, kwh));
          }
          if (ld > 0) {
            ndfl = 1;
          } else {
            ndfl = ndfl + 1;
          }
        }
        if (it > itmax) {
          info = kbot;
        }
      }
      WORK[offsetWORK] = lwkopt;
      return info;
    }
    module.exports = dlaqr4;
  }
});

// lib/lapack/base/dlaqr3/lib/base.js
var require_base48 = __commonJS({
  "lib/lapack/base/dlaqr3/lib/base.js"(exports, module) {
    "use strict";
    var dlaqr23impl = require_base44().dlaqr23impl;
    var dlaqr4 = require_base47();
    function dlaqr3(wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork) {
      return dlaqr23impl(dlaqr4, wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork);
    }
    module.exports = dlaqr3;
  }
});

// lib/lapack/base/dlaqr0/lib/base.js
var require_base49 = __commonJS({
  "lib/lapack/base/dlaqr0/lib/base.js"(exports, module) {
    "use strict";
    var dlahqr = require_base34();
    var dlacpy = require_base9();
    var dlanv2 = require_base33();
    var dlaqr3 = require_base48();
    var dlaqr4 = require_base47();
    var dlaqr5 = require_base46();
    var ZERO = 0;
    var ONE = 1;
    var NTINY = 15;
    var KEXNW = 5;
    var KEXSH = 6;
    var WILK1 = 0.75;
    var WILK2 = -0.4375;
    var IPARMQ_NMIN = 75;
    var IPARMQ_NIBBLE = 14;
    var IPARMQ_K22MIN = 14;
    var IPARMQ_KACMIN = 14;
    var IPARMQ_KNWSWP = 500;
    function iparmqShifts(nh) {
      var ns = 2;
      if (nh >= 30) {
        ns = 4;
      }
      if (nh >= 60) {
        ns = 10;
      }
      if (nh >= 150) {
        ns = Math.max(10, Math.round(nh / (Math.log(nh) / Math.log(2))));
      }
      if (nh >= 590) {
        ns = 64;
      }
      if (nh >= 3e3) {
        ns = 128;
      }
      if (nh >= 6e3) {
        ns = 256;
      }
      ns = Math.max(2, ns - ns % 2);
      return ns;
    }
    function dlaqr0(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork) {
      var lwkopt;
      var nwupbd;
      var nsmax;
      var nwmax;
      var sorted;
      var itmax;
      var kwtop;
      var kbot;
      var ktop;
      var ndfl;
      var ndec;
      var info;
      var swap;
      var nmin;
      var nwr;
      var nsr;
      var nw;
      var nh;
      var nho;
      var nve;
      var ns;
      var ks;
      var ld;
      var ls;
      var kv;
      var kt;
      var kwv;
      var kwh;
      var ku;
      var kdu;
      var aa;
      var bb;
      var cc;
      var dd;
      var ss;
      var lv2;
      var it;
      var k;
      var i;
      var nibble;
      var kacc22;
      var inf;
      var aedResult;
      var zdum;
      function hij(r, c) {
        return offsetH + (r - 1) * strideH1 + (c - 1) * strideH2;
      }
      info = 0;
      if (N === 0) {
        WORK[offsetWORK] = ONE;
        return 0;
      }
      if (N <= NTINY) {
        lwkopt = 1;
        if (lwork !== -1) {
          info = dlahqr(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ);
        }
      } else {
        info = 0;
        ns = iparmqShifts(ihi - ilo + 1);
        nwr = ihi - ilo + 1 <= IPARMQ_KNWSWP ? ns : Math.floor(3 * ns / 2);
        nwr = Math.max(2, nwr);
        nwr = Math.min(ihi - ilo + 1, Math.floor((N - 1) / 3), nwr);
        nsr = ns;
        nsr = Math.min(nsr, Math.floor((N - 3) / 6), ihi - ilo);
        nsr = Math.max(2, nsr - nsr % 2);
        dlaqr3(wantt, wantz, N, ilo, ihi, nwr + 1, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WR, strideWR, offsetWR, WI, strideWI, offsetWI, H, strideH1, strideH2, offsetH, N, H, strideH1, strideH2, offsetH, N, H, strideH1, strideH2, offsetH, WORK, strideWORK, offsetWORK, -1);
        lwkopt = Math.max(Math.floor(3 * nsr / 2), Math.floor(WORK[offsetWORK]));
        if (lwork === -1) {
          WORK[offsetWORK] = lwkopt;
          return 0;
        }
        nmin = Math.max(NTINY, IPARMQ_NMIN);
        nibble = IPARMQ_NIBBLE;
        kacc22 = 0;
        if (ns >= IPARMQ_KACMIN) {
          kacc22 = 1;
        }
        if (ns >= IPARMQ_K22MIN) {
          kacc22 = 2;
        }
        kacc22 = Math.max(0, Math.min(2, kacc22));
        nwmax = Math.min(Math.floor((N - 1) / 3), Math.floor(lwork / 2));
        nw = nwmax;
        nsmax = Math.min(Math.floor((N - 3) / 6), Math.floor(2 * lwork / 3));
        nsmax = nsmax - nsmax % 2;
        ndfl = 1;
        itmax = Math.max(30, 2 * KEXSH) * Math.max(10, ihi - ilo + 1);
        kbot = ihi;
        for (it = 1; it <= itmax; it++) {
          if (kbot < ilo) {
            info = 0;
            break;
          }
          for (k = kbot; k >= ilo + 1; k--) {
            if (H[hij(k, k - 1)] === ZERO) {
              break;
            }
          }
          if (k > ilo && H[hij(k, k - 1)] !== ZERO) {
            k = ilo;
          }
          ktop = k;
          nh = kbot - ktop + 1;
          nwupbd = Math.min(nh, nwmax);
          if (ndfl < KEXNW) {
            nw = Math.min(nwupbd, nwr);
          } else {
            nw = Math.min(nwupbd, 2 * nw);
          }
          if (nw < nwmax) {
            if (nw >= nh - 1) {
              nw = nh;
            } else {
              kwtop = kbot - nw + 1;
              if (Math.abs(H[hij(kwtop, kwtop - 1)]) > Math.abs(H[hij(kwtop - 1, kwtop - 2)])) {
                nw = nw + 1;
              }
            }
          }
          if (ndfl < KEXNW) {
            ndec = -1;
          } else if (ndec >= 0 || nw >= nwupbd) {
            ndec = ndec + 1;
            if (nw - ndec < 2) {
              ndec = 0;
            }
            nw = nw - ndec;
          }
          kv = N - nw + 1;
          kt = nw + 1;
          nho = N - nw - 1 - kt + 1;
          kwv = nw + 2;
          nve = N - nw - kwv + 1;
          aedResult = dlaqr3(wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WR, strideWR, offsetWR, WI, strideWI, offsetWI, H, strideH1, strideH2, hij(kv, 1), nho, H, strideH1, strideH2, hij(kv, kt), nve, H, strideH1, strideH2, hij(kwv, 1), WORK, strideWORK, offsetWORK, lwork);
          ls = aedResult.ns;
          ld = aedResult.nd;
          kbot = kbot - ld;
          ks = kbot - ls + 1;
          if (ld === 0 || 100 * ld <= nw * nibble && kbot - ktop + 1 > Math.min(nmin, nwmax)) {
            ns = Math.min(nsmax, nsr, Math.max(2, kbot - ktop));
            ns = ns - ns % 2;
            if (ndfl % KEXSH === 0) {
              ks = kbot - ns + 1;
              for (i = kbot; i >= Math.max(ks + 1, ktop + 2); i -= 2) {
                ss = Math.abs(H[hij(i, i - 1)]) + Math.abs(H[hij(i - 1, i - 2)]);
                aa = WILK1 * ss + H[hij(i, i)];
                bb = ss;
                cc = WILK2 * ss;
                dd = aa;
                lv2 = dlanv2(aa, bb, cc, dd);
                WR[offsetWR + (i - 2) * strideWR] = lv2.rt1r;
                WI[offsetWI + (i - 2) * strideWI] = lv2.rt1i;
                WR[offsetWR + (i - 1) * strideWR] = lv2.rt2r;
                WI[offsetWI + (i - 1) * strideWI] = lv2.rt2i;
              }
              if (ks === ktop) {
                WR[offsetWR + ks * strideWR] = H[hij(ks + 1, ks + 1)];
                WI[offsetWI + ks * strideWI] = ZERO;
                WR[offsetWR + (ks - 1) * strideWR] = WR[offsetWR + ks * strideWR];
                WI[offsetWI + (ks - 1) * strideWI] = WI[offsetWI + ks * strideWI];
              }
            } else {
              if (kbot - ks + 1 <= Math.floor(ns / 2)) {
                ks = kbot - ns + 1;
                kt = N - ns + 1;
                dlacpy("full", ns, ns, H, strideH1, strideH2, hij(ks, ks), H, strideH1, strideH2, hij(kt, 1));
                if (ns > nmin) {
                  inf = dlaqr4(false, false, ns, 1, ns, H, strideH1, strideH2, hij(kt, 1), WR, strideWR, offsetWR + (ks - 1) * strideWR, WI, strideWI, offsetWI + (ks - 1) * strideWI, 1, 1, WORK, 1, 1, 0, WORK, strideWORK, offsetWORK, lwork);
                } else {
                  zdum = new Float64Array(1);
                  inf = dlahqr(false, false, ns, 1, ns, H, strideH1, strideH2, hij(kt, 1), WR, strideWR, offsetWR + (ks - 1) * strideWR, WI, strideWI, offsetWI + (ks - 1) * strideWI, 1, 1, zdum, 1, 1, 0);
                }
                ks = ks + inf;
                if (ks >= kbot) {
                  aa = H[hij(kbot - 1, kbot - 1)];
                  cc = H[hij(kbot, kbot - 1)];
                  bb = H[hij(kbot - 1, kbot)];
                  dd = H[hij(kbot, kbot)];
                  lv2 = dlanv2(aa, bb, cc, dd);
                  WR[offsetWR + (kbot - 2) * strideWR] = lv2.rt1r;
                  WI[offsetWI + (kbot - 2) * strideWI] = lv2.rt1i;
                  WR[offsetWR + (kbot - 1) * strideWR] = lv2.rt2r;
                  WI[offsetWI + (kbot - 1) * strideWI] = lv2.rt2i;
                  ks = kbot - 1;
                }
              }
              if (kbot - ks + 1 > ns) {
                sorted = false;
                for (k = kbot; k >= ks + 1; k--) {
                  if (sorted) {
                    break;
                  }
                  sorted = true;
                  for (i = ks; i <= k - 1; i++) {
                    if (Math.abs(WR[offsetWR + (i - 1) * strideWR]) + Math.abs(WI[offsetWI + (i - 1) * strideWI]) < Math.abs(WR[offsetWR + i * strideWR]) + Math.abs(WI[offsetWI + i * strideWI])) {
                      sorted = false;
                      swap = WR[offsetWR + (i - 1) * strideWR];
                      WR[offsetWR + (i - 1) * strideWR] = WR[offsetWR + i * strideWR];
                      WR[offsetWR + i * strideWR] = swap;
                      swap = WI[offsetWI + (i - 1) * strideWI];
                      WI[offsetWI + (i - 1) * strideWI] = WI[offsetWI + i * strideWI];
                      WI[offsetWI + i * strideWI] = swap;
                    }
                  }
                }
              }
              for (i = kbot; i >= ks + 2; i -= 2) {
                if (WI[offsetWI + (i - 1) * strideWI] !== -WI[offsetWI + (i - 2) * strideWI]) {
                  swap = WR[offsetWR + (i - 1) * strideWR];
                  WR[offsetWR + (i - 1) * strideWR] = WR[offsetWR + (i - 2) * strideWR];
                  WR[offsetWR + (i - 2) * strideWR] = WR[offsetWR + (i - 3) * strideWR];
                  WR[offsetWR + (i - 3) * strideWR] = swap;
                  swap = WI[offsetWI + (i - 1) * strideWI];
                  WI[offsetWI + (i - 1) * strideWI] = WI[offsetWI + (i - 2) * strideWI];
                  WI[offsetWI + (i - 2) * strideWI] = WI[offsetWI + (i - 3) * strideWI];
                  WI[offsetWI + (i - 3) * strideWI] = swap;
                }
              }
            }
            if (kbot - ks + 1 === 2) {
              if (WI[offsetWI + (kbot - 1) * strideWI] === ZERO) {
                if (Math.abs(WR[offsetWR + (kbot - 1) * strideWR] - H[hij(kbot, kbot)]) < Math.abs(WR[offsetWR + (kbot - 2) * strideWR] - H[hij(kbot, kbot)])) {
                  WR[offsetWR + (kbot - 2) * strideWR] = WR[offsetWR + (kbot - 1) * strideWR];
                } else {
                  WR[offsetWR + (kbot - 1) * strideWR] = WR[offsetWR + (kbot - 2) * strideWR];
                }
              }
            }
            ns = Math.min(ns, kbot - ks + 1);
            ns = ns - ns % 2;
            ks = kbot - ns + 1;
            kdu = 2 * ns;
            ku = N - kdu + 1;
            kwh = kdu + 1;
            nho = N - kdu + 1 - 4 - (kdu + 1) + 1;
            kwv = kdu + 4;
            nve = N - kdu - kwv + 1;
            dlaqr5(wantt, wantz, kacc22, N, ktop - 1, kbot - 1, ns, WR, strideWR, offsetWR + (ks - 1) * strideWR, WI, strideWI, offsetWI + (ks - 1) * strideWI, H, strideH1, strideH2, offsetH, iloz - 1, ihiz - 1, Z, strideZ1, strideZ2, offsetZ, WORK, 1, 3, offsetWORK, H, strideH1, strideH2, hij(ku, 1), nve, H, strideH1, strideH2, hij(kwv, 1), nho, H, strideH1, strideH2, hij(ku, kwh));
          }
          if (ld > 0) {
            ndfl = 1;
          } else {
            ndfl = ndfl + 1;
          }
        }
        if (it > itmax) {
          info = kbot;
        }
      }
      WORK[offsetWORK] = lwkopt;
      return info;
    }
    module.exports = dlaqr0;
  }
});

// lib/lapack/base/dhseqr/lib/base.js
var require_base50 = __commonJS({
  "lib/lapack/base/dhseqr/lib/base.js"(exports, module) {
    "use strict";
    var dlacpy = require_base9();
    var dlahqr = require_base34();
    var dlaqr0 = require_base49();
    var dlaset = require_base35();
    var ZERO = 0;
    var ONE = 1;
    var NTINY = 15;
    var NL = 49;
    var NMIN = 75;
    function dhseqr(job, compz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, Z, strideZ1, strideZ2, offsetZ) {
      var wantt;
      var initz;
      var wantz;
      var info;
      var kbot;
      var WORKL;
      var HL;
      var i;
      wantt = job === "schur";
      initz = compz === "initialize";
      wantz = initz || compz === "update";
      if (N === 0) {
        return 0;
      }
      for (i = 0; i < ilo - 1; i++) {
        WR[offsetWR + i * strideWR] = H[offsetH + i * strideH1 + i * strideH2];
        WI[offsetWI + i * strideWI] = ZERO;
      }
      for (i = ihi; i < N; i++) {
        WR[offsetWR + i * strideWR] = H[offsetH + i * strideH1 + i * strideH2];
        WI[offsetWI + i * strideWI] = ZERO;
      }
      if (initz) {
        dlaset("all", N, N, ZERO, ONE, Z, strideZ1, strideZ2, offsetZ);
      }
      if (ilo === ihi) {
        WR[offsetWR + (ilo - 1) * strideWR] = H[offsetH + (ilo - 1) * strideH1 + (ilo - 1) * strideH2];
        WI[offsetWI + (ilo - 1) * strideWI] = ZERO;
        return 0;
      }
      if (N > Math.max(NTINY, NMIN)) {
        WORKL = new Float64Array(Math.max(1, N));
        info = dlaqr0(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, ilo, ihi, Z, strideZ1, strideZ2, offsetZ, WORKL, 1, 0, WORKL.length);
      } else {
        info = dlahqr(wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, ilo, ihi, Z, strideZ1, strideZ2, offsetZ);
        if (info > 0) {
          kbot = info;
          if (N >= NL) {
            WORKL = new Float64Array(Math.max(1, N));
            info = dlaqr0(wantt, wantz, N, ilo, kbot, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, ilo, ihi, Z, strideZ1, strideZ2, offsetZ, WORKL, 1, 0, WORKL.length);
          } else {
            HL = new Float64Array(NL * NL);
            WORKL = new Float64Array(NL);
            dlacpy("all", N, N, H, strideH1, strideH2, offsetH, HL, 1, NL, 0);
            HL[N + (N - 1) * NL] = ZERO;
            dlaset("full", NL, NL - N, ZERO, ZERO, HL, 1, NL, N * NL);
            info = dlaqr0(wantt, wantz, NL, ilo, kbot, HL, 1, NL, 0, WR, strideWR, offsetWR, WI, strideWI, offsetWI, ilo, ihi, Z, strideZ1, strideZ2, offsetZ, WORKL, 1, 0, NL);
            if (wantt || info !== 0) {
              dlacpy("all", N, N, HL, 1, NL, 0, H, strideH1, strideH2, offsetH);
            }
          }
        }
      }
      if ((wantt || info !== 0) && N > 2) {
        dlaset("lower", N - 2, N - 2, ZERO, ZERO, H, strideH1, strideH2, offsetH + 2 * strideH1 + 0 * strideH2);
      }
      return info;
    }
    module.exports = dhseqr;
  }
});

// lib/blas/base/ddot/lib/base.js
var require_base51 = __commonJS({
  "lib/blas/base/ddot/lib/base.js"(exports, module) {
    "use strict";
    var M = 5;
    function ddot(N, x, strideX, offsetX, y, strideY, offsetY) {
      var dtemp;
      var ix;
      var iy;
      var m;
      var i;
      dtemp = 0;
      if (N <= 0) {
        return dtemp;
      }
      ix = offsetX;
      iy = offsetY;
      if (strideX === 1 && strideY === 1) {
        m = N % M;
        if (m > 0) {
          for (i = 0; i < m; i++) {
            dtemp += x[ix] * y[iy];
            ix += 1;
            iy += 1;
          }
        }
        if (N < M) {
          return dtemp;
        }
        for (i = m; i < N; i += M) {
          dtemp += x[ix] * y[iy] + x[ix + 1] * y[iy + 1] + x[ix + 2] * y[iy + 2] + x[ix + 3] * y[iy + 3] + x[ix + 4] * y[iy + 4];
          ix += M;
          iy += M;
        }
        return dtemp;
      }
      for (i = 0; i < N; i++) {
        dtemp += x[ix] * y[iy];
        ix += strideX;
        iy += strideY;
      }
      return dtemp;
    }
    module.exports = ddot;
  }
});

// lib/lapack/base/dladiv/lib/base.js
var require_base52 = __commonJS({
  "lib/lapack/base/dladiv/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var BS = 2;
    var HALF = 0.5;
    var TWO = 2;
    var OV = dlamch("overflow");
    var UN = dlamch("underflow");
    var EPS = dlamch("epsilon");
    var BE = BS / (EPS * EPS);
    var HALF_OV = HALF * OV;
    var SCALE_THRESH = UN * BS / EPS;
    function dladiv2(a, b, c, d, r, t) {
      var br;
      if (r !== 0) {
        br = b * r;
        if (br !== 0) {
          return (a + br) * t;
        }
        return a * t + b * t * r;
      }
      return (a + d * (b / c)) * t;
    }
    function dladiv1(a, b, c, d, out) {
      var r;
      var t;
      r = d / c;
      t = 1 / (c + d * r);
      out[0] = dladiv2(a, b, c, d, r, t);
      out[1] = dladiv2(b, -a, c, d, r, t);
    }
    function dladiv(a, b, c, d, out) {
      var aa;
      var bb;
      var cc;
      var dd;
      var ab;
      var cd;
      var s;
      aa = a;
      bb = b;
      cc = c;
      dd = d;
      ab = Math.max(Math.abs(a), Math.abs(b));
      cd = Math.max(Math.abs(c), Math.abs(d));
      s = 1;
      if (ab >= HALF_OV) {
        aa *= HALF;
        bb *= HALF;
        s *= TWO;
      }
      if (cd >= HALF_OV) {
        cc *= HALF;
        dd *= HALF;
        s *= HALF;
      }
      if (ab <= SCALE_THRESH) {
        aa *= BE;
        bb *= BE;
        s /= BE;
      }
      if (cd <= SCALE_THRESH) {
        cc *= BE;
        dd *= BE;
        s *= BE;
      }
      if (Math.abs(dd) <= Math.abs(cc)) {
        dladiv1(aa, bb, cc, dd, out);
      } else {
        dladiv1(bb, aa, dd, cc, out);
        out[1] = -out[1];
      }
      out[0] *= s;
      out[1] *= s;
      return out;
    }
    module.exports = dladiv;
  }
});

// lib/lapack/base/dlaln2/lib/base.js
var require_base53 = __commonJS({
  "lib/lapack/base/dlaln2/lib/base.js"(exports, module) {
    "use strict";
    var dladiv = require_base52();
    var dlamch = require_base5();
    var ZERO = 0;
    var ONE = 1;
    var TWO = 2;
    var SMLNUM = TWO * dlamch("safe-minimum");
    var BIGNUM = ONE / SMLNUM;
    var ZSWAP = [false, false, true, true];
    var RSWAP = [false, true, false, true];
    var IPIVOT = [
      0,
      1,
      2,
      3,
      1,
      0,
      3,
      2,
      2,
      3,
      0,
      1,
      3,
      2,
      1,
      0
    ];
    var DIVOUT = new Float64Array(2);
    function dlaln2(ltrans, na, nw, smin, ca, A, strideA1, strideA2, offsetA, d1, d2, B, strideB1, strideB2, offsetB, wr, wi, X, strideX1, strideX2, offsetX) {
      var u22abs;
      var ur11r;
      var ui11r;
      var scale;
      var xnorm;
      var smini;
      var icmax;
      var cnorm;
      var bnorm;
      var ur12s;
      var ui12s;
      var info;
      var temp;
      var bbnd;
      var cmax;
      var ur11;
      var ur12;
      var ur22;
      var ui11;
      var ui12;
      var ui22;
      var cr21;
      var cr22;
      var ci21;
      var ci22;
      var lr21;
      var li21;
      var br1;
      var br2;
      var bi1;
      var bi2;
      var xr1;
      var xr2;
      var xi1;
      var xi2;
      var csr;
      var csi;
      var cr;
      var ci;
      var j;
      smini = Math.max(smin, SMLNUM);
      info = 0;
      scale = ONE;
      if (na === 1) {
        if (nw === 1) {
          csr = ca * A[offsetA] - wr * d1;
          cnorm = Math.abs(csr);
          if (cnorm < smini) {
            csr = smini;
            cnorm = smini;
            info = 1;
          }
          bnorm = Math.abs(B[offsetB]);
          if (cnorm < ONE && bnorm > ONE) {
            if (bnorm > BIGNUM * cnorm) {
              scale = ONE / bnorm;
            }
          }
          X[offsetX] = B[offsetB] * scale / csr;
          xnorm = Math.abs(X[offsetX]);
        } else {
          csr = ca * A[offsetA] - wr * d1;
          csi = -wi * d1;
          cnorm = Math.abs(csr) + Math.abs(csi);
          if (cnorm < smini) {
            csr = smini;
            csi = ZERO;
            cnorm = smini;
            info = 1;
          }
          bnorm = Math.abs(B[offsetB]) + Math.abs(B[offsetB + strideB2]);
          if (cnorm < ONE && bnorm > ONE) {
            if (bnorm > BIGNUM * cnorm) {
              scale = ONE / bnorm;
            }
          }
          dladiv(scale * B[offsetB], scale * B[offsetB + strideB2], csr, csi, DIVOUT);
          X[offsetX] = DIVOUT[0];
          X[offsetX + strideX2] = DIVOUT[1];
          xnorm = Math.abs(X[offsetX]) + Math.abs(X[offsetX + strideX2]);
        }
      } else {
        cr = new Float64Array(4);
        cr[0] = ca * A[offsetA] - wr * d1;
        cr[3] = ca * A[offsetA + strideA1 + strideA2] - wr * d2;
        if (ltrans) {
          cr[2] = ca * A[offsetA + strideA1];
          cr[1] = ca * A[offsetA + strideA2];
        } else {
          cr[1] = ca * A[offsetA + strideA1];
          cr[2] = ca * A[offsetA + strideA2];
        }
        if (nw === 1) {
          cmax = ZERO;
          icmax = -1;
          for (j = 0; j < 4; j++) {
            if (Math.abs(cr[j]) > cmax) {
              cmax = Math.abs(cr[j]);
              icmax = j;
            }
          }
          if (cmax < smini) {
            bnorm = Math.max(Math.abs(B[offsetB]), Math.abs(B[offsetB + strideB1]));
            if (smini < ONE && bnorm > ONE) {
              if (bnorm > BIGNUM * smini) {
                scale = ONE / bnorm;
              }
            }
            temp = scale / smini;
            X[offsetX] = temp * B[offsetB];
            X[offsetX + strideX1] = temp * B[offsetB + strideB1];
            xnorm = temp * bnorm;
            info = 1;
            return { "info": info, "scale": scale, "xnorm": xnorm };
          }
          ur11 = cr[icmax];
          cr21 = cr[IPIVOT[1 + icmax * 4]];
          ur12 = cr[IPIVOT[2 + icmax * 4]];
          cr22 = cr[IPIVOT[3 + icmax * 4]];
          ur11r = ONE / ur11;
          lr21 = ur11r * cr21;
          ur22 = cr22 - ur12 * lr21;
          if (Math.abs(ur22) < smini) {
            ur22 = smini;
            info = 1;
          }
          if (RSWAP[icmax]) {
            br1 = B[offsetB + strideB1];
            br2 = B[offsetB];
          } else {
            br1 = B[offsetB];
            br2 = B[offsetB + strideB1];
          }
          br2 = br2 - lr21 * br1;
          bbnd = Math.max(Math.abs(br1 * (ur22 * ur11r)), Math.abs(br2));
          if (bbnd > ONE && Math.abs(ur22) < ONE) {
            if (bbnd >= BIGNUM * Math.abs(ur22)) {
              scale = ONE / bbnd;
            }
          }
          xr2 = br2 * scale / ur22;
          xr1 = scale * br1 * ur11r - xr2 * (ur11r * ur12);
          if (ZSWAP[icmax]) {
            X[offsetX] = xr2;
            X[offsetX + strideX1] = xr1;
          } else {
            X[offsetX] = xr1;
            X[offsetX + strideX1] = xr2;
          }
          xnorm = Math.max(Math.abs(xr1), Math.abs(xr2));
          if (xnorm > ONE && cmax > ONE) {
            if (xnorm > BIGNUM / cmax) {
              temp = cmax / BIGNUM;
              X[offsetX] = temp * X[offsetX];
              X[offsetX + strideX1] = temp * X[offsetX + strideX1];
              xnorm = temp * xnorm;
              scale = temp * scale;
            }
          }
        } else {
          ci = new Float64Array(4);
          ci[0] = -wi * d1;
          ci[1] = ZERO;
          ci[2] = ZERO;
          ci[3] = -wi * d2;
          cmax = ZERO;
          icmax = -1;
          for (j = 0; j < 4; j++) {
            if (Math.abs(cr[j]) + Math.abs(ci[j]) > cmax) {
              cmax = Math.abs(cr[j]) + Math.abs(ci[j]);
              icmax = j;
            }
          }
          if (cmax < smini) {
            bnorm = Math.max(
              Math.abs(B[offsetB]) + Math.abs(B[offsetB + strideB2]),
              Math.abs(B[offsetB + strideB1]) + Math.abs(B[offsetB + strideB1 + strideB2])
            );
            if (smini < ONE && bnorm > ONE) {
              if (bnorm > BIGNUM * smini) {
                scale = ONE / bnorm;
              }
            }
            temp = scale / smini;
            X[offsetX] = temp * B[offsetB];
            X[offsetX + strideX1] = temp * B[offsetB + strideB1];
            X[offsetX + strideX2] = temp * B[offsetB + strideB2];
            X[offsetX + strideX1 + strideX2] = temp * B[offsetB + strideB1 + strideB2];
            xnorm = temp * bnorm;
            info = 1;
            return { "info": info, "scale": scale, "xnorm": xnorm };
          }
          ur11 = cr[icmax];
          ui11 = ci[icmax];
          cr21 = cr[IPIVOT[1 + icmax * 4]];
          ci21 = ci[IPIVOT[1 + icmax * 4]];
          ur12 = cr[IPIVOT[2 + icmax * 4]];
          ui12 = ci[IPIVOT[2 + icmax * 4]];
          cr22 = cr[IPIVOT[3 + icmax * 4]];
          ci22 = ci[IPIVOT[3 + icmax * 4]];
          if (icmax === 0 || icmax === 3) {
            if (Math.abs(ur11) > Math.abs(ui11)) {
              temp = ui11 / ur11;
              ur11r = ONE / (ur11 * (ONE + temp * temp));
              ui11r = -temp * ur11r;
            } else {
              temp = ur11 / ui11;
              ui11r = -ONE / (ui11 * (ONE + temp * temp));
              ur11r = -temp * ui11r;
            }
            lr21 = cr21 * ur11r;
            li21 = cr21 * ui11r;
            ur12s = ur12 * ur11r;
            ui12s = ur12 * ui11r;
            ur22 = cr22 - ur12 * lr21;
            ui22 = ci22 - ur12 * li21;
          } else {
            ur11r = ONE / ur11;
            ui11r = ZERO;
            lr21 = cr21 * ur11r;
            li21 = ci21 * ur11r;
            ur12s = ur12 * ur11r;
            ui12s = ui12 * ur11r;
            ur22 = cr22 - ur12 * lr21 + ui12 * li21;
            ui22 = -ur12 * li21 - ui12 * lr21;
          }
          u22abs = Math.abs(ur22) + Math.abs(ui22);
          if (u22abs < smini) {
            ur22 = smini;
            ui22 = ZERO;
            info = 1;
          }
          if (RSWAP[icmax]) {
            br2 = B[offsetB];
            br1 = B[offsetB + strideB1];
            bi2 = B[offsetB + strideB2];
            bi1 = B[offsetB + strideB1 + strideB2];
          } else {
            br1 = B[offsetB];
            br2 = B[offsetB + strideB1];
            bi1 = B[offsetB + strideB2];
            bi2 = B[offsetB + strideB1 + strideB2];
          }
          br2 = br2 - lr21 * br1 + li21 * bi1;
          bi2 = bi2 - li21 * br1 - lr21 * bi1;
          bbnd = Math.max(
            (Math.abs(br1) + Math.abs(bi1)) * (u22abs * (Math.abs(ur11r) + Math.abs(ui11r))),
            Math.abs(br2) + Math.abs(bi2)
          );
          if (bbnd > ONE && u22abs < ONE) {
            if (bbnd >= BIGNUM * u22abs) {
              scale = ONE / bbnd;
              br1 = scale * br1;
              bi1 = scale * bi1;
              br2 = scale * br2;
              bi2 = scale * bi2;
            }
          }
          dladiv(br2, bi2, ur22, ui22, DIVOUT);
          xr2 = DIVOUT[0];
          xi2 = DIVOUT[1];
          xr1 = ur11r * br1 - ui11r * bi1 - ur12s * xr2 + ui12s * xi2;
          xi1 = ui11r * br1 + ur11r * bi1 - ui12s * xr2 - ur12s * xi2;
          if (ZSWAP[icmax]) {
            X[offsetX] = xr2;
            X[offsetX + strideX1] = xr1;
            X[offsetX + strideX2] = xi2;
            X[offsetX + strideX1 + strideX2] = xi1;
          } else {
            X[offsetX] = xr1;
            X[offsetX + strideX1] = xr2;
            X[offsetX + strideX2] = xi1;
            X[offsetX + strideX1 + strideX2] = xi2;
          }
          xnorm = Math.max(Math.abs(xr1) + Math.abs(xi1), Math.abs(xr2) + Math.abs(xi2));
          if (xnorm > ONE && cmax > ONE) {
            if (xnorm > BIGNUM / cmax) {
              temp = cmax / BIGNUM;
              X[offsetX] = temp * X[offsetX];
              X[offsetX + strideX1] = temp * X[offsetX + strideX1];
              X[offsetX + strideX2] = temp * X[offsetX + strideX2];
              X[offsetX + strideX1 + strideX2] = temp * X[offsetX + strideX1 + strideX2];
              xnorm = temp * xnorm;
              scale = temp * scale;
            }
          }
        }
      }
      return { "info": info, "scale": scale, "xnorm": xnorm };
    }
    module.exports = dlaln2;
  }
});

// lib/lapack/base/dtrevc3/lib/base.js
var require_base54 = __commonJS({
  "lib/lapack/base/dtrevc3/lib/base.js"(exports, module) {
    "use strict";
    var daxpy = require_base13();
    var dcopy = require_base23();
    var ddot = require_base51();
    var dgemv = require_base14();
    var dscal = require_base();
    var idamax = require_base3();
    var dlaln2 = require_base53();
    var dlamch = require_base5();
    var ZERO = 0;
    var ONE = 1;
    var UNFL = dlamch("safe-minimum");
    var ULP = dlamch("precision");
    var SMLNUM = UNFL * (1 / ULP);
    var BIGNUM = (ONE - ULP) / SMLNUM;
    var X = new Float64Array(4);
    function dtrevc3(side, howmny, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, lwork) {
      var rightv;
      var bothv;
      var leftv;
      var allv;
      var over;
      var somev;
      var smin;
      var remax;
      var emax;
      var scale;
      var xnorm;
      var vcrit;
      var vmax;
      var beta;
      var pair;
      var rec;
      var wr;
      var wi;
      var ip;
      var is;
      var ki;
      var ii;
      var jnxt;
      var j;
      var j1;
      var j2;
      var k;
      var m;
      var res;
      var sT1;
      var sT2;
      var oT;
      var nb;
      sT1 = strideT1;
      sT2 = strideT2;
      oT = offsetT;
      bothv = side === "both";
      rightv = side === "right" || bothv;
      leftv = side === "left" || bothv;
      allv = howmny === "all";
      over = howmny === "backtransform";
      somev = howmny === "selected";
      nb = 1;
      if (N === 0) {
        return 0;
      }
      WORK[offsetWORK] = ZERO;
      for (j = 1; j < N; j++) {
        WORK[offsetWORK + j] = ZERO;
        for (k = 0; k < j; k++) {
          WORK[offsetWORK + j] += Math.abs(T[oT + k * sT1 + j * sT2]);
        }
      }
      if (somev) {
        m = 0;
        pair = false;
        for (j = 0; j < N; j++) {
          if (pair) {
            pair = false;
          } else {
            if (j < N - 1) {
              if (T[oT + (j + 1) * sT1 + j * sT2] === ZERO) {
                if (SELECT[offsetSELECT + j * strideSELECT]) {
                  m += 1;
                }
              } else {
                pair = true;
                if (SELECT[offsetSELECT + j * strideSELECT] || SELECT[offsetSELECT + (j + 1) * strideSELECT]) {
                  m += 2;
                }
              }
            } else {
              if (SELECT[offsetSELECT + j * strideSELECT]) {
                m += 1;
              }
            }
          }
        }
      } else {
        m = N;
      }
      if (rightv) {
        ip = 0;
        is = m - 1;
        for (ki = N - 1; ki >= 0; ki--) {
          if (ip === -1) {
            ip = 1;
            continue;
          } else if (ki === 0) {
            ip = 0;
          } else if (T[oT + ki * sT1 + (ki - 1) * sT2] === ZERO) {
            ip = 0;
          } else {
            ip = -1;
          }
          if (somev) {
            if (ip === 0) {
              if (!SELECT[offsetSELECT + ki * strideSELECT]) {
                continue;
              }
            } else {
              if (!SELECT[offsetSELECT + (ki - 1) * strideSELECT]) {
                continue;
              }
            }
          }
          wr = T[oT + ki * sT1 + ki * sT2];
          wi = ZERO;
          if (ip !== 0) {
            wi = Math.sqrt(Math.abs(T[oT + ki * sT1 + (ki - 1) * sT2])) * Math.sqrt(Math.abs(T[oT + (ki - 1) * sT1 + ki * sT2]));
          }
          smin = Math.max(ULP * (Math.abs(wr) + Math.abs(wi)), SMLNUM);
          if (ip === 0) {
            WORK[offsetWORK + 2 * N + ki] = ONE;
            for (k = 0; k < ki; k++) {
              WORK[offsetWORK + 2 * N + k] = -T[oT + k * sT1 + ki * sT2];
            }
            jnxt = ki - 1;
            for (j = ki - 1; j >= 0; j--) {
              if (j > jnxt) {
                continue;
              }
              j1 = j;
              j2 = j;
              jnxt = j - 1;
              if (j > 0) {
                if (T[oT + j * sT1 + (j - 1) * sT2] !== ZERO) {
                  j1 = j - 1;
                  jnxt = j - 2;
                }
              }
              if (j1 === j2) {
                res = dlaln2(
                  false,
                  1,
                  1,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + 2 * N + j,
                  wr,
                  ZERO,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                xnorm = res.xnorm;
                if (xnorm > ONE) {
                  if (WORK[offsetWORK + j] > BIGNUM / xnorm) {
                    X[0] = X[0] / xnorm;
                    scale = scale / xnorm;
                  }
                }
                if (scale !== ONE) {
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + 2 * N);
                }
                WORK[offsetWORK + 2 * N + j] = X[0];
                daxpy(j, -X[0], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + 2 * N);
              } else {
                res = dlaln2(
                  false,
                  2,
                  1,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + (j - 1) * sT1 + (j - 1) * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + 2 * N + (j - 1),
                  wr,
                  ZERO,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                xnorm = res.xnorm;
                if (xnorm > ONE) {
                  beta = Math.max(WORK[offsetWORK + j - 1], WORK[offsetWORK + j]);
                  if (beta > BIGNUM / xnorm) {
                    X[0] = X[0] / xnorm;
                    X[1] = X[1] / xnorm;
                    scale = scale / xnorm;
                  }
                }
                if (scale !== ONE) {
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + 2 * N);
                }
                WORK[offsetWORK + 2 * N + j - 1] = X[0];
                WORK[offsetWORK + 2 * N + j] = X[1];
                daxpy(j - 1, -X[0], T, sT1, oT + (j - 1) * sT2, WORK, 1, offsetWORK + 2 * N);
                daxpy(j - 1, -X[1], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + 2 * N);
              }
            }
            if (!over) {
              dcopy(ki + 1, WORK, 1, offsetWORK + 2 * N, VR, strideVR1, offsetVR + is * strideVR2);
              ii = idamax(ki + 1, VR, strideVR1, offsetVR + is * strideVR2);
              remax = ONE / Math.abs(VR[offsetVR + ii * strideVR1 + is * strideVR2]);
              dscal(ki + 1, remax, VR, strideVR1, offsetVR + is * strideVR2);
              for (k = ki + 1; k < N; k++) {
                VR[offsetVR + k * strideVR1 + is * strideVR2] = ZERO;
              }
            } else {
              if (ki > 0) {
                dgemv(
                  "no-transpose",
                  N,
                  ki,
                  ONE,
                  VR,
                  strideVR1,
                  strideVR2,
                  offsetVR,
                  WORK,
                  1,
                  offsetWORK + 2 * N,
                  WORK[offsetWORK + 2 * N + ki],
                  VR,
                  strideVR1,
                  offsetVR + ki * strideVR2
                );
              } else {
                dscal(N, WORK[offsetWORK + 2 * N + ki], VR, strideVR1, offsetVR + ki * strideVR2);
              }
              ii = idamax(N, VR, strideVR1, offsetVR + ki * strideVR2);
              remax = ONE / Math.abs(VR[offsetVR + ii * strideVR1 + ki * strideVR2]);
              dscal(N, remax, VR, strideVR1, offsetVR + ki * strideVR2);
            }
          } else {
            if (Math.abs(T[oT + (ki - 1) * sT1 + ki * sT2]) >= Math.abs(T[oT + ki * sT1 + (ki - 1) * sT2])) {
              WORK[offsetWORK + N + ki - 1] = ONE;
              WORK[offsetWORK + 2 * N + ki] = wi / T[oT + (ki - 1) * sT1 + ki * sT2];
            } else {
              WORK[offsetWORK + N + ki - 1] = -wi / T[oT + ki * sT1 + (ki - 1) * sT2];
              WORK[offsetWORK + 2 * N + ki] = ONE;
            }
            WORK[offsetWORK + N + ki] = ZERO;
            WORK[offsetWORK + 2 * N + ki - 1] = ZERO;
            for (k = 0; k < ki - 1; k++) {
              WORK[offsetWORK + N + k] = -WORK[offsetWORK + N + ki - 1] * T[oT + k * sT1 + (ki - 1) * sT2];
              WORK[offsetWORK + 2 * N + k] = -WORK[offsetWORK + 2 * N + ki] * T[oT + k * sT1 + ki * sT2];
            }
            jnxt = ki - 2;
            for (j = ki - 2; j >= 0; j--) {
              if (j > jnxt) {
                continue;
              }
              j1 = j;
              j2 = j;
              jnxt = j - 1;
              if (j > 0) {
                if (T[oT + j * sT1 + (j - 1) * sT2] !== ZERO) {
                  j1 = j - 1;
                  jnxt = j - 2;
                }
              }
              if (j1 === j2) {
                res = dlaln2(
                  false,
                  1,
                  2,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j,
                  wr,
                  wi,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                xnorm = res.xnorm;
                if (xnorm > ONE) {
                  if (WORK[offsetWORK + j] > BIGNUM / xnorm) {
                    X[0] = X[0] / xnorm;
                    X[2] = X[2] / xnorm;
                    scale = scale / xnorm;
                  }
                }
                if (scale !== ONE) {
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + N);
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + 2 * N);
                }
                WORK[offsetWORK + N + j] = X[0];
                WORK[offsetWORK + 2 * N + j] = X[2];
                daxpy(j, -X[0], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + N);
                daxpy(j, -X[2], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + 2 * N);
              } else {
                res = dlaln2(
                  false,
                  2,
                  2,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + (j - 1) * sT1 + (j - 1) * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j - 1,
                  wr,
                  wi,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                xnorm = res.xnorm;
                if (xnorm > ONE) {
                  beta = Math.max(WORK[offsetWORK + j - 1], WORK[offsetWORK + j]);
                  if (beta > BIGNUM / xnorm) {
                    rec = ONE / xnorm;
                    X[0] *= rec;
                    X[2] *= rec;
                    X[1] *= rec;
                    X[3] *= rec;
                    scale *= rec;
                  }
                }
                if (scale !== ONE) {
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + N);
                  dscal(ki + 1, scale, WORK, 1, offsetWORK + 2 * N);
                }
                WORK[offsetWORK + N + j - 1] = X[0];
                WORK[offsetWORK + N + j] = X[1];
                WORK[offsetWORK + 2 * N + j - 1] = X[2];
                WORK[offsetWORK + 2 * N + j] = X[3];
                daxpy(j - 1, -X[0], T, sT1, oT + (j - 1) * sT2, WORK, 1, offsetWORK + N);
                daxpy(j - 1, -X[1], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + N);
                daxpy(j - 1, -X[2], T, sT1, oT + (j - 1) * sT2, WORK, 1, offsetWORK + 2 * N);
                daxpy(j - 1, -X[3], T, sT1, oT + j * sT2, WORK, 1, offsetWORK + 2 * N);
              }
            }
            if (!over) {
              dcopy(ki + 1, WORK, 1, offsetWORK + N, VR, strideVR1, offsetVR + (is - 1) * strideVR2);
              dcopy(ki + 1, WORK, 1, offsetWORK + 2 * N, VR, strideVR1, offsetVR + is * strideVR2);
              emax = ZERO;
              for (k = 0; k <= ki; k++) {
                emax = Math.max(emax, Math.abs(VR[offsetVR + k * strideVR1 + (is - 1) * strideVR2]) + Math.abs(VR[offsetVR + k * strideVR1 + is * strideVR2]));
              }
              remax = ONE / emax;
              dscal(ki + 1, remax, VR, strideVR1, offsetVR + (is - 1) * strideVR2);
              dscal(ki + 1, remax, VR, strideVR1, offsetVR + is * strideVR2);
              for (k = ki + 1; k < N; k++) {
                VR[offsetVR + k * strideVR1 + (is - 1) * strideVR2] = ZERO;
                VR[offsetVR + k * strideVR1 + is * strideVR2] = ZERO;
              }
            } else {
              if (ki > 1) {
                dgemv(
                  "no-transpose",
                  N,
                  ki - 1,
                  ONE,
                  VR,
                  strideVR1,
                  strideVR2,
                  offsetVR,
                  WORK,
                  1,
                  offsetWORK + N,
                  WORK[offsetWORK + N + ki - 1],
                  VR,
                  strideVR1,
                  offsetVR + (ki - 1) * strideVR2
                );
                dgemv(
                  "no-transpose",
                  N,
                  ki - 1,
                  ONE,
                  VR,
                  strideVR1,
                  strideVR2,
                  offsetVR,
                  WORK,
                  1,
                  offsetWORK + 2 * N,
                  WORK[offsetWORK + 2 * N + ki],
                  VR,
                  strideVR1,
                  offsetVR + ki * strideVR2
                );
              } else {
                dscal(N, WORK[offsetWORK + N + ki - 1], VR, strideVR1, offsetVR + (ki - 1) * strideVR2);
                dscal(N, WORK[offsetWORK + 2 * N + ki], VR, strideVR1, offsetVR + ki * strideVR2);
              }
              emax = ZERO;
              for (k = 0; k < N; k++) {
                emax = Math.max(emax, Math.abs(VR[offsetVR + k * strideVR1 + (ki - 1) * strideVR2]) + Math.abs(VR[offsetVR + k * strideVR1 + ki * strideVR2]));
              }
              remax = ONE / emax;
              dscal(N, remax, VR, strideVR1, offsetVR + (ki - 1) * strideVR2);
              dscal(N, remax, VR, strideVR1, offsetVR + ki * strideVR2);
            }
          }
          is -= 1;
          if (ip !== 0) {
            is -= 1;
          }
        }
      }
      if (leftv) {
        ip = 0;
        is = 0;
        for (ki = 0; ki < N; ki++) {
          if (ip === 1) {
            ip = -1;
            continue;
          } else if (ki === N - 1) {
            ip = 0;
          } else if (T[oT + (ki + 1) * sT1 + ki * sT2] === ZERO) {
            ip = 0;
          } else {
            ip = 1;
          }
          if (somev) {
            if (!SELECT[offsetSELECT + ki * strideSELECT]) {
              continue;
            }
          }
          wr = T[oT + ki * sT1 + ki * sT2];
          wi = ZERO;
          if (ip !== 0) {
            wi = Math.sqrt(Math.abs(T[oT + ki * sT1 + (ki + 1) * sT2])) * Math.sqrt(Math.abs(T[oT + (ki + 1) * sT1 + ki * sT2]));
          }
          smin = Math.max(ULP * (Math.abs(wr) + Math.abs(wi)), SMLNUM);
          if (ip === 0) {
            WORK[offsetWORK + N + ki] = ONE;
            for (k = ki + 1; k < N; k++) {
              WORK[offsetWORK + N + k] = -T[oT + ki * sT1 + k * sT2];
            }
            vmax = ONE;
            vcrit = BIGNUM;
            jnxt = ki + 1;
            for (j = ki + 1; j < N; j++) {
              if (j < jnxt) {
                continue;
              }
              j1 = j;
              j2 = j;
              jnxt = j + 1;
              if (j < N - 1) {
                if (T[oT + (j + 1) * sT1 + j * sT2] !== ZERO) {
                  j2 = j + 1;
                  jnxt = j + 2;
                }
              }
              if (j1 === j2) {
                if (WORK[offsetWORK + j] > vcrit) {
                  rec = ONE / vmax;
                  dscal(N - ki, rec, WORK, 1, offsetWORK + N + ki);
                  vmax = ONE;
                  vcrit = BIGNUM;
                }
                WORK[offsetWORK + N + j] -= ddot(
                  j - ki - 1,
                  T,
                  sT1,
                  oT + (ki + 1) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 1
                );
                res = dlaln2(
                  false,
                  1,
                  1,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j,
                  wr,
                  ZERO,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                if (scale !== ONE) {
                  dscal(N - ki, scale, WORK, 1, offsetWORK + N + ki);
                }
                WORK[offsetWORK + N + j] = X[0];
                vmax = Math.max(Math.abs(WORK[offsetWORK + N + j]), vmax);
                vcrit = BIGNUM / vmax;
              } else {
                beta = Math.max(WORK[offsetWORK + j], WORK[offsetWORK + j + 1]);
                if (beta > vcrit) {
                  rec = ONE / vmax;
                  dscal(N - ki, rec, WORK, 1, offsetWORK + N + ki);
                  vmax = ONE;
                  vcrit = BIGNUM;
                }
                WORK[offsetWORK + N + j] -= ddot(
                  j - ki - 1,
                  T,
                  sT1,
                  oT + (ki + 1) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 1
                );
                WORK[offsetWORK + N + j + 1] -= ddot(
                  j - ki - 1,
                  T,
                  sT1,
                  oT + (ki + 1) * sT1 + (j + 1) * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 1
                );
                res = dlaln2(
                  true,
                  2,
                  1,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j,
                  wr,
                  ZERO,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                if (scale !== ONE) {
                  dscal(N - ki, scale, WORK, 1, offsetWORK + N + ki);
                }
                WORK[offsetWORK + N + j] = X[0];
                WORK[offsetWORK + N + j + 1] = X[1];
                vmax = Math.max(
                  Math.abs(WORK[offsetWORK + N + j]),
                  Math.abs(WORK[offsetWORK + N + j + 1]),
                  vmax
                );
                vcrit = BIGNUM / vmax;
              }
            }
            if (!over) {
              dcopy(N - ki, WORK, 1, offsetWORK + N + ki, VL, strideVL1, offsetVL + ki * strideVL1 + is * strideVL2);
              ii = idamax(N - ki, VL, strideVL1, offsetVL + ki * strideVL1 + is * strideVL2) + ki;
              remax = ONE / Math.abs(VL[offsetVL + ii * strideVL1 + is * strideVL2]);
              dscal(N - ki, remax, VL, strideVL1, offsetVR + ki * strideVL1 + is * strideVL2);
              for (k = 0; k < ki; k++) {
                VL[offsetVL + k * strideVL1 + is * strideVL2] = ZERO;
              }
            } else {
              if (ki < N - 1) {
                dgemv(
                  "no-transpose",
                  N,
                  N - ki - 1,
                  ONE,
                  VL,
                  strideVL1,
                  strideVL2,
                  offsetVL + (ki + 1) * strideVL2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 1,
                  WORK[offsetWORK + N + ki],
                  VL,
                  strideVL1,
                  offsetVL + ki * strideVL2
                );
              } else {
                dscal(N, WORK[offsetWORK + N + ki], VL, strideVL1, offsetVL + ki * strideVL2);
              }
              ii = idamax(N, VL, strideVL1, offsetVL + ki * strideVL2);
              remax = ONE / Math.abs(VL[offsetVL + ii * strideVL1 + ki * strideVL2]);
              dscal(N, remax, VL, strideVL1, offsetVL + ki * strideVL2);
            }
          } else {
            if (Math.abs(T[oT + ki * sT1 + (ki + 1) * sT2]) >= Math.abs(T[oT + (ki + 1) * sT1 + ki * sT2])) {
              WORK[offsetWORK + N + ki] = wi / T[oT + ki * sT1 + (ki + 1) * sT2];
              WORK[offsetWORK + 2 * N + ki + 1] = ONE;
            } else {
              WORK[offsetWORK + N + ki] = ONE;
              WORK[offsetWORK + 2 * N + ki + 1] = -wi / T[oT + (ki + 1) * sT1 + ki * sT2];
            }
            WORK[offsetWORK + N + ki + 1] = ZERO;
            WORK[offsetWORK + 2 * N + ki] = ZERO;
            for (k = ki + 2; k < N; k++) {
              WORK[offsetWORK + N + k] = -WORK[offsetWORK + N + ki] * T[oT + ki * sT1 + k * sT2];
              WORK[offsetWORK + 2 * N + k] = -WORK[offsetWORK + 2 * N + ki + 1] * T[oT + (ki + 1) * sT1 + k * sT2];
            }
            vmax = ONE;
            vcrit = BIGNUM;
            jnxt = ki + 2;
            for (j = ki + 2; j < N; j++) {
              if (j < jnxt) {
                continue;
              }
              j1 = j;
              j2 = j;
              jnxt = j + 1;
              if (j < N - 1) {
                if (T[oT + (j + 1) * sT1 + j * sT2] !== ZERO) {
                  j2 = j + 1;
                  jnxt = j + 2;
                }
              }
              if (j1 === j2) {
                if (WORK[offsetWORK + j] > vcrit) {
                  rec = ONE / vmax;
                  dscal(N - ki, rec, WORK, 1, offsetWORK + N + ki);
                  dscal(N - ki, rec, WORK, 1, offsetWORK + 2 * N + ki);
                  vmax = ONE;
                  vcrit = BIGNUM;
                }
                WORK[offsetWORK + N + j] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 2
                );
                WORK[offsetWORK + 2 * N + j] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + 2 * N + ki + 2
                );
                res = dlaln2(
                  false,
                  1,
                  2,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j,
                  wr,
                  -wi,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                if (scale !== ONE) {
                  dscal(N - ki, scale, WORK, 1, offsetWORK + N + ki);
                  dscal(N - ki, scale, WORK, 1, offsetWORK + 2 * N + ki);
                }
                WORK[offsetWORK + N + j] = X[0];
                WORK[offsetWORK + 2 * N + j] = X[2];
                vmax = Math.max(
                  Math.abs(WORK[offsetWORK + N + j]),
                  Math.abs(WORK[offsetWORK + 2 * N + j]),
                  vmax
                );
                vcrit = BIGNUM / vmax;
              } else {
                beta = Math.max(WORK[offsetWORK + j], WORK[offsetWORK + j + 1]);
                if (beta > vcrit) {
                  rec = ONE / vmax;
                  dscal(N - ki, rec, WORK, 1, offsetWORK + N + ki);
                  dscal(N - ki, rec, WORK, 1, offsetWORK + 2 * N + ki);
                  vmax = ONE;
                  vcrit = BIGNUM;
                }
                WORK[offsetWORK + N + j] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 2
                );
                WORK[offsetWORK + 2 * N + j] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + j * sT2,
                  WORK,
                  1,
                  offsetWORK + 2 * N + ki + 2
                );
                WORK[offsetWORK + N + j + 1] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + (j + 1) * sT2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 2
                );
                WORK[offsetWORK + 2 * N + j + 1] -= ddot(
                  j - ki - 2,
                  T,
                  sT1,
                  oT + (ki + 2) * sT1 + (j + 1) * sT2,
                  WORK,
                  1,
                  offsetWORK + 2 * N + ki + 2
                );
                res = dlaln2(
                  true,
                  2,
                  2,
                  smin,
                  ONE,
                  T,
                  sT1,
                  sT2,
                  oT + j * sT1 + j * sT2,
                  ONE,
                  ONE,
                  WORK,
                  1,
                  N,
                  offsetWORK + N + j,
                  wr,
                  -wi,
                  X,
                  1,
                  2,
                  0
                );
                scale = res.scale;
                if (scale !== ONE) {
                  dscal(N - ki, scale, WORK, 1, offsetWORK + N + ki);
                  dscal(N - ki, scale, WORK, 1, offsetWORK + 2 * N + ki);
                }
                WORK[offsetWORK + N + j] = X[0];
                WORK[offsetWORK + N + j + 1] = X[1];
                WORK[offsetWORK + 2 * N + j] = X[2];
                WORK[offsetWORK + 2 * N + j + 1] = X[3];
                vmax = Math.max(
                  Math.abs(X[0]),
                  Math.abs(X[2]),
                  Math.abs(X[1]),
                  Math.abs(X[3]),
                  vmax
                );
                vcrit = BIGNUM / vmax;
              }
            }
            if (!over) {
              dcopy(N - ki, WORK, 1, offsetWORK + N + ki, VL, strideVL1, offsetVL + ki * strideVL1 + is * strideVL2);
              dcopy(N - ki, WORK, 1, offsetWORK + 2 * N + ki, VL, strideVL1, offsetVL + ki * strideVL1 + (is + 1) * strideVL2);
              emax = ZERO;
              for (k = ki; k < N; k++) {
                emax = Math.max(emax, Math.abs(VL[offsetVL + k * strideVL1 + is * strideVL2]) + Math.abs(VL[offsetVL + k * strideVL1 + (is + 1) * strideVL2]));
              }
              remax = ONE / emax;
              dscal(N - ki, remax, VL, strideVL1, offsetVL + ki * strideVL1 + is * strideVL2);
              dscal(N - ki, remax, VL, strideVL1, offsetVL + ki * strideVL1 + (is + 1) * strideVL2);
              for (k = 0; k < ki; k++) {
                VL[offsetVL + k * strideVL1 + is * strideVL2] = ZERO;
                VL[offsetVL + k * strideVL1 + (is + 1) * strideVL2] = ZERO;
              }
            } else {
              if (ki < N - 2) {
                dgemv(
                  "no-transpose",
                  N,
                  N - ki - 2,
                  ONE,
                  VL,
                  strideVL1,
                  strideVL2,
                  offsetVL + (ki + 2) * strideVL2,
                  WORK,
                  1,
                  offsetWORK + N + ki + 2,
                  WORK[offsetWORK + N + ki],
                  VL,
                  strideVL1,
                  offsetVL + ki * strideVL2
                );
                dgemv(
                  "no-transpose",
                  N,
                  N - ki - 2,
                  ONE,
                  VL,
                  strideVL1,
                  strideVL2,
                  offsetVL + (ki + 2) * strideVL2,
                  WORK,
                  1,
                  offsetWORK + 2 * N + ki + 2,
                  WORK[offsetWORK + 2 * N + ki + 1],
                  VL,
                  strideVL1,
                  offsetVL + (ki + 1) * strideVL2
                );
              } else {
                dscal(N, WORK[offsetWORK + N + ki], VL, strideVL1, offsetVL + ki * strideVL2);
                dscal(N, WORK[offsetWORK + 2 * N + ki + 1], VL, strideVL1, offsetVL + (ki + 1) * strideVL2);
              }
              emax = ZERO;
              for (k = 0; k < N; k++) {
                emax = Math.max(emax, Math.abs(VL[offsetVL + k * strideVL1 + ki * strideVL2]) + Math.abs(VL[offsetVL + k * strideVL1 + (ki + 1) * strideVL2]));
              }
              remax = ONE / emax;
              dscal(N, remax, VL, strideVL1, offsetVL + ki * strideVL2);
              dscal(N, remax, VL, strideVL1, offsetVL + (ki + 1) * strideVL2);
            }
          }
          is += 1;
          if (ip !== 0) {
            is += 1;
          }
        }
      }
      return 0;
    }
    module.exports = dtrevc3;
  }
});

// lib/lapack/base/dgeev/lib/base.js
var require_base55 = __commonJS({
  "lib/lapack/base/dgeev/lib/base.js"(exports, module) {
    "use strict";
    var dscal = require_base();
    var dnrm2 = require_base2();
    var idamax = require_base3();
    var drot = require_base4();
    var dlamch = require_base5();
    var dlange = require_base7();
    var dlascl = require_base8();
    var dlacpy = require_base9();
    var dgebal = require_base11();
    var dgebak = require_base12();
    var dgehrd = require_base28();
    var dorghr = require_base32();
    var dhseqr = require_base50();
    var dlartg = require_base40();
    var dlapy2 = require_base19();
    var dtrevc3 = require_base54();
    var ZERO = 0;
    var ONE = 1;
    var EPS = dlamch("precision");
    var SMLNUM_BASE = dlamch("safe-minimum");
    var BIGNUM_BASE = ONE / SMLNUM_BASE;
    var SMLNUM = Math.sqrt(SMLNUM_BASE) / EPS;
    var BIGNUM = ONE / SMLNUM;
    var DLARTG_OUT = new Float64Array(3);
    function dgeev2(jobvl, jobvr, N, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR) {
      var wantvl;
      var wantvr;
      var scalea;
      var cscale;
      var anrm;
      var info;
      var ilo;
      var ihi;
      var WORK;
      var SCALE;
      var TAU;
      var SELECT;
      var side;
      var scl;
      var cs;
      var sn;
      var r;
      var bal;
      var nout;
      var k;
      var i;
      wantvl = jobvl === "compute-vectors";
      wantvr = jobvr === "compute-vectors";
      if (N === 0) {
        return 0;
      }
      if (N === 1) {
        WR[offsetWR] = A[offsetA];
        WI[offsetWI] = ZERO;
        if (wantvl) {
          VL[offsetVL] = ONE;
        }
        if (wantvr) {
          VR[offsetVR] = ONE;
        }
        return 0;
      }
      SCALE = new Float64Array(N);
      TAU = new Float64Array(N);
      WORK = new Float64Array(Math.max(4 * N, 1));
      SELECT = new Uint8Array(1);
      anrm = dlange("max", N, N, A, strideA1, strideA2, offsetA, WORK, 1, 0);
      scalea = false;
      cscale = 0;
      if (anrm > ZERO && anrm < SMLNUM) {
        scalea = true;
        cscale = SMLNUM;
      } else if (anrm > BIGNUM) {
        scalea = true;
        cscale = BIGNUM;
      }
      if (scalea) {
        dlascl("general", 0, 0, anrm, cscale, N, N, A, strideA1, strideA2, offsetA);
      }
      bal = dgebal("both", N, A, strideA1, strideA2, offsetA, SCALE, 1, 0);
      ilo = bal.ilo;
      ihi = bal.ihi;
      dgehrd(N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, 1, 0, WORK, 1, 0);
      if (wantvl) {
        side = "left";
        dlacpy("lower", N, N, A, strideA1, strideA2, offsetA, VL, strideVL1, strideVL2, offsetVL);
        dorghr(N, ilo, ihi, VL, strideVL1, strideVL2, offsetVL, TAU, 1, 0, WORK, 1, 0, WORK.length);
        info = dhseqr("schur", "update", N, ilo, ihi, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VL, strideVL1, strideVL2, offsetVL);
        if (wantvr) {
          side = "both";
          dlacpy("full", N, N, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR);
        }
      } else if (wantvr) {
        side = "right";
        dlacpy("lower", N, N, A, strideA1, strideA2, offsetA, VR, strideVR1, strideVR2, offsetVR);
        dorghr(N, ilo, ihi, VR, strideVR1, strideVR2, offsetVR, TAU, 1, 0, WORK, 1, 0, WORK.length);
        info = dhseqr("schur", "update", N, ilo, ihi, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VR, strideVR1, strideVR2, offsetVR);
      } else {
        info = dhseqr("eigenvalues", "none", N, ilo, ihi, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VR, strideVR1, strideVR2, offsetVR);
      }
      if (info !== 0) {
        if (scalea) {
          dlascl("general", 0, 0, cscale, anrm, N - info, 1, WR, strideWR, 1, offsetWR + info * strideWR);
          dlascl("general", 0, 0, cscale, anrm, N - info, 1, WI, strideWI, 1, offsetWI + info * strideWI);
          if (info > 0) {
            dlascl("general", 0, 0, cscale, anrm, ilo - 1, 1, WR, strideWR, 1, offsetWR);
            dlascl("general", 0, 0, cscale, anrm, ilo - 1, 1, WI, strideWI, 1, offsetWI);
          }
        }
        return info;
      }
      if (wantvl || wantvr) {
        var TREVC_WORK = new Float64Array(3 * N);
        nout = 0;
        dtrevc3(
          side,
          "backtransform",
          SELECT,
          1,
          0,
          N,
          A,
          strideA1,
          strideA2,
          offsetA,
          VL,
          strideVL1,
          strideVL2,
          offsetVL,
          VR,
          strideVR1,
          strideVR2,
          offsetVR,
          N,
          nout,
          TREVC_WORK,
          1,
          0,
          3 * N
        );
      }
      if (wantvl) {
        dgebak("both", "left", N, ilo, ihi, SCALE, 1, 0, N, VL, strideVL1, strideVL2, offsetVL);
        for (i = 0; i < N; i++) {
          if (WI[offsetWI + i * strideWI] === ZERO) {
            scl = ONE / dnrm2(N, VL, strideVL1, offsetVL + i * strideVL2);
            dscal(N, scl, VL, strideVL1, offsetVL + i * strideVL2);
          } else if (WI[offsetWI + i * strideWI] > ZERO) {
            scl = ONE / dlapy2(
              dnrm2(N, VL, strideVL1, offsetVL + i * strideVL2),
              dnrm2(N, VL, strideVL1, offsetVL + (i + 1) * strideVL2)
            );
            dscal(N, scl, VL, strideVL1, offsetVL + i * strideVL2);
            dscal(N, scl, VL, strideVL1, offsetVL + (i + 1) * strideVL2);
            for (k = 0; k < N; k++) {
              WORK[k] = VL[offsetVL + k * strideVL1 + i * strideVL2] * VL[offsetVL + k * strideVL1 + i * strideVL2] + VL[offsetVL + k * strideVL1 + (i + 1) * strideVL2] * VL[offsetVL + k * strideVL1 + (i + 1) * strideVL2];
            }
            k = idamax(N, WORK, 1, 0);
            dlartg(
              VL[offsetVL + k * strideVL1 + i * strideVL2],
              VL[offsetVL + k * strideVL1 + (i + 1) * strideVL2],
              DLARTG_OUT
            );
            cs = DLARTG_OUT[0];
            sn = DLARTG_OUT[1];
            drot(
              N,
              VL,
              strideVL1,
              offsetVL + i * strideVL2,
              VL,
              strideVL1,
              offsetVL + (i + 1) * strideVL2,
              cs,
              sn
            );
            VL[offsetVL + k * strideVL1 + (i + 1) * strideVL2] = ZERO;
          }
        }
      }
      if (wantvr) {
        dgebak("both", "right", N, ilo, ihi, SCALE, 1, 0, N, VR, strideVR1, strideVR2, offsetVR);
        for (i = 0; i < N; i++) {
          if (WI[offsetWI + i * strideWI] === ZERO) {
            scl = ONE / dnrm2(N, VR, strideVR1, offsetVR + i * strideVR2);
            dscal(N, scl, VR, strideVR1, offsetVR + i * strideVR2);
          } else if (WI[offsetWI + i * strideWI] > ZERO) {
            scl = ONE / dlapy2(
              dnrm2(N, VR, strideVR1, offsetVR + i * strideVR2),
              dnrm2(N, VR, strideVR1, offsetVR + (i + 1) * strideVR2)
            );
            dscal(N, scl, VR, strideVR1, offsetVR + i * strideVR2);
            dscal(N, scl, VR, strideVR1, offsetVR + (i + 1) * strideVR2);
            for (k = 0; k < N; k++) {
              WORK[k] = VR[offsetVR + k * strideVR1 + i * strideVR2] * VR[offsetVR + k * strideVR1 + i * strideVR2] + VR[offsetVR + k * strideVR1 + (i + 1) * strideVR2] * VR[offsetVR + k * strideVR1 + (i + 1) * strideVR2];
            }
            k = idamax(N, WORK, 1, 0);
            dlartg(
              VR[offsetVR + k * strideVR1 + i * strideVR2],
              VR[offsetVR + k * strideVR1 + (i + 1) * strideVR2],
              DLARTG_OUT
            );
            cs = DLARTG_OUT[0];
            sn = DLARTG_OUT[1];
            drot(
              N,
              VR,
              strideVR1,
              offsetVR + i * strideVR2,
              VR,
              strideVR1,
              offsetVR + (i + 1) * strideVR2,
              cs,
              sn
            );
            VR[offsetVR + k * strideVR1 + (i + 1) * strideVR2] = ZERO;
          }
        }
      }
      if (scalea) {
        dlascl("general", 0, 0, cscale, anrm, N - info, 1, WR, strideWR, 1, offsetWR + info * strideWR);
        dlascl("general", 0, 0, cscale, anrm, N - info, 1, WI, strideWI, 1, offsetWI + info * strideWI);
        if (info > 0) {
          dlascl("general", 0, 0, cscale, anrm, ilo - 1, 1, WR, strideWR, 1, offsetWR);
          dlascl("general", 0, 0, cscale, anrm, ilo - 1, 1, WI, strideWI, 1, offsetWI);
        }
      }
      return info;
    }
    module.exports = dgeev2;
  }
});

// lib/lapack/base/dgeqr2/lib/base.js
var require_base56 = __commonJS({
  "lib/lapack/base/dgeqr2/lib/base.js"(exports, module) {
    "use strict";
    var dlarfg = require_base20();
    var dlarf = require_base18();
    function dgeqr2(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var alpha;
      var aii;
      var K;
      var i;
      K = Math.min(M, N);
      for (i = 0; i < K; i++) {
        aii = offsetA + i * strideA1 + i * strideA2;
        dlarfg(
          M - i,
          A,
          aii,
          A,
          strideA1,
          offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
          TAU,
          offsetTAU + i * strideTAU
        );
        if (i < N - 1) {
          alpha = A[aii];
          A[aii] = 1;
          dlarf(
            "left",
            M - i,
            N - i - 1,
            A,
            strideA1,
            aii,
            TAU[offsetTAU + i * strideTAU],
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + (i + 1) * strideA2,
            WORK,
            strideWORK,
            offsetWORK
          );
          A[aii] = alpha;
        }
      }
      return 0;
    }
    module.exports = dgeqr2;
  }
});

// lib/lapack/base/dgeqrf/lib/base.js
var require_base57 = __commonJS({
  "lib/lapack/base/dgeqrf/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dgeqr2 = require_base56();
    var dlarfb = require_base27();
    var dlarft = require_base30();
    var DEFAULT_NB = 32;
    function dgeqrf(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var ldwork;
      var nbmin;
      var iws;
      var ib;
      var nb;
      var nx;
      var T;
      var K;
      var i;
      K = Math.min(M, N);
      if (K === 0) {
        return 0;
      }
      nb = DEFAULT_NB;
      nbmin = 2;
      nx = 0;
      iws = N;
      if (nb > 1 && nb < K) {
        nx = 0;
        if (nx < K) {
          ldwork = N;
          iws = ldwork * nb;
        }
      }
      T = new Float64Array2(nb * nb);
      if (!WORK || WORK.length < iws) {
        WORK = new Float64Array2(iws);
        offsetWORK = 0;
        strideWORK = 1;
      }
      ldwork = N;
      if (nb >= nbmin && nb < K && nx < K) {
        i = 0;
        while (i <= K - 1 - nx) {
          ib = Math.min(K - i, nb);
          dgeqr2(
            M - i,
            ib,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + i * strideA2,
            TAU,
            strideTAU,
            offsetTAU + i * strideTAU,
            WORK,
            strideWORK,
            offsetWORK
          );
          if (i + ib < N) {
            dlarft(
              "forward",
              "columnwise",
              M - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAU,
              strideTAU,
              offsetTAU + i * strideTAU,
              T,
              1,
              nb,
              0
            );
            dlarfb(
              "left",
              "transpose",
              "forward",
              "columnwise",
              M - i,
              N - i - ib,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              T,
              1,
              nb,
              0,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + ib) * strideA2,
              WORK,
              1,
              ldwork,
              offsetWORK
            );
          }
          i += nb;
        }
      } else {
        i = 0;
      }
      if (i <= K - 1) {
        dgeqr2(
          M - i,
          N - i,
          A,
          strideA1,
          strideA2,
          offsetA + i * strideA1 + i * strideA2,
          TAU,
          strideTAU,
          offsetTAU + i * strideTAU,
          WORK,
          strideWORK,
          offsetWORK
        );
      }
      return 0;
    }
    module.exports = dgeqrf;
  }
});

// lib/lapack/base/dgelq2/lib/base.js
var require_base58 = __commonJS({
  "lib/lapack/base/dgelq2/lib/base.js"(exports, module) {
    "use strict";
    var dlarfg = require_base20();
    var dlarf = require_base18();
    function dgelq2(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var save;
      var aii;
      var K;
      var i;
      K = Math.min(M, N);
      for (i = 0; i < K; i++) {
        aii = offsetA + i * strideA1 + i * strideA2;
        dlarfg(
          N - i,
          A,
          aii,
          A,
          strideA2,
          offsetA + i * strideA1 + Math.min(i + 1, N - 1) * strideA2,
          TAU,
          offsetTAU + i * strideTAU
        );
        if (i < M - 1) {
          save = A[aii];
          A[aii] = 1;
          dlarf(
            "right",
            M - i - 1,
            // number of rows of sub-matrix
            N - i,
            // number of columns of sub-matrix
            A,
            strideA2,
            aii,
            // v = row i from col i onward, stride along columns
            TAU[offsetTAU + i * strideTAU],
            // tau is a plain scalar for dlarf
            A,
            strideA1,
            strideA2,
            offsetA + (i + 1) * strideA1 + i * strideA2,
            // C = A(i+1, i)
            WORK,
            strideWORK,
            offsetWORK
          );
          A[aii] = save;
        }
      }
      return 0;
    }
    module.exports = dgelq2;
  }
});

// lib/lapack/base/dgelqf/lib/base.js
var require_base59 = __commonJS({
  "lib/lapack/base/dgelqf/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dgelq2 = require_base58();
    var dlarfb = require_base27();
    var dlarft = require_base30();
    var DEFAULT_NB = 32;
    function dgelqf(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var ldwork;
      var nbmin;
      var iws;
      var ib;
      var nb;
      var nx;
      var T;
      var K;
      var i;
      K = Math.min(M, N);
      if (K === 0) {
        return 0;
      }
      nb = DEFAULT_NB;
      nbmin = 2;
      nx = 0;
      iws = M;
      if (nb > 1 && nb < K) {
        nx = 0;
        if (nx < K) {
          ldwork = M;
          iws = ldwork * nb;
        }
      }
      T = new Float64Array2(nb * nb);
      if (!WORK || WORK.length < iws) {
        WORK = new Float64Array2(iws);
        offsetWORK = 0;
        strideWORK = 1;
      }
      ldwork = M;
      if (nb >= nbmin && nb < K && nx < K) {
        i = 0;
        while (i <= K - 1 - nx) {
          ib = Math.min(K - i, nb);
          dgelq2(
            ib,
            N - i,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + i * strideA2,
            TAU,
            strideTAU,
            offsetTAU + i * strideTAU,
            WORK,
            strideWORK,
            offsetWORK
          );
          if (i + ib < M) {
            dlarft(
              "forward",
              "rowwise",
              N - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAU,
              strideTAU,
              offsetTAU + i * strideTAU,
              T,
              1,
              nb,
              0
            );
            dlarfb(
              "right",
              "no-transpose",
              "forward",
              "rowwise",
              M - i - ib,
              N - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              T,
              1,
              nb,
              0,
              A,
              strideA1,
              strideA2,
              offsetA + (i + ib) * strideA1 + i * strideA2,
              WORK,
              1,
              ldwork,
              offsetWORK
            );
          }
          i += nb;
        }
      } else {
        i = 0;
      }
      if (i <= K - 1) {
        dgelq2(
          M - i,
          N - i,
          A,
          strideA1,
          strideA2,
          offsetA + i * strideA1 + i * strideA2,
          TAU,
          strideTAU,
          offsetTAU + i * strideTAU,
          WORK,
          strideWORK,
          offsetWORK
        );
      }
      return 0;
    }
    module.exports = dgelqf;
  }
});

// lib/lapack/base/dorgl2/lib/base.js
var require_base60 = __commonJS({
  "lib/lapack/base/dorgl2/lib/base.js"(exports, module) {
    "use strict";
    var dlarf = require_base18();
    var dscal = require_base();
    function dorgl2(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var it;
      var i;
      var j;
      var l;
      if (M <= 0) {
        return 0;
      }
      if (K < M) {
        for (j = 0; j < N; j++) {
          for (l = K; l < M; l++) {
            A[offsetA + l * strideA1 + j * strideA2] = 0;
          }
          if (j >= K && j < M) {
            A[offsetA + j * strideA1 + j * strideA2] = 1;
          }
        }
      }
      for (i = K - 1; i >= 0; i--) {
        it = offsetTAU + i * strideTAU;
        if (i < N - 1) {
          if (i < M - 1) {
            A[offsetA + i * strideA1 + i * strideA2] = 1;
            dlarf(
              "right",
              M - i - 1,
              N - i,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAU[it],
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          dscal(
            N - i - 1,
            -TAU[it],
            A,
            strideA2,
            offsetA + i * strideA1 + (i + 1) * strideA2
          );
        }
        A[offsetA + i * strideA1 + i * strideA2] = 1 - TAU[it];
        for (l = 0; l < i; l++) {
          A[offsetA + i * strideA1 + l * strideA2] = 0;
        }
      }
      return 0;
    }
    module.exports = dorgl2;
  }
});

// lib/lapack/base/dorglq/lib/base.js
var require_base61 = __commonJS({
  "lib/lapack/base/dorglq/lib/base.js"(exports, module) {
    "use strict";
    var dorgl2 = require_base60();
    var dlarft = require_base30();
    var dlarfb = require_base27();
    var NB = 32;
    function dorglq(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var ldwork;
      var nb;
      var nx;
      var kk;
      var ki;
      var ib;
      var i;
      var j;
      var l;
      if (M <= 0) {
        return 0;
      }
      nb = NB;
      nx = 0;
      ldwork = M;
      if (nb >= 2 && nb < K) {
        nx = 0;
        if (nx < K) {
          ki = Math.floor((K - nx - 1) / nb) * nb;
          kk = Math.min(K, ki + nb);
          for (j = 0; j < kk; j++) {
            for (i = kk; i < M; i++) {
              A[offsetA + i * strideA1 + j * strideA2] = 0;
            }
          }
        }
      } else {
        kk = 0;
      }
      if (kk < M) {
        dorgl2(
          M - kk,
          N - kk,
          K - kk,
          A,
          strideA1,
          strideA2,
          offsetA + kk * strideA1 + kk * strideA2,
          TAU,
          strideTAU,
          offsetTAU + kk * strideTAU,
          WORK,
          strideWORK,
          offsetWORK
        );
      }
      if (kk > 0) {
        for (i = ki; i >= 0; i -= nb) {
          ib = Math.min(nb, K - i);
          if (i + ib < M) {
            dlarft(
              "forward",
              "rowwise",
              N - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAU,
              strideTAU,
              offsetTAU + i * strideTAU,
              WORK,
              1,
              ldwork,
              offsetWORK
            );
            dlarfb(
              "right",
              "transpose",
              "forward",
              "rowwise",
              M - i - ib,
              N - i,
              ib,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              WORK,
              1,
              ldwork,
              offsetWORK,
              A,
              strideA1,
              strideA2,
              offsetA + (i + ib) * strideA1 + i * strideA2,
              WORK,
              1,
              ldwork,
              offsetWORK + ib
            );
          }
          dorgl2(
            ib,
            N - i,
            ib,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + i * strideA2,
            TAU,
            strideTAU,
            offsetTAU + i * strideTAU,
            WORK,
            strideWORK,
            offsetWORK
          );
          for (j = 0; j < i; j++) {
            for (l = i; l < i + ib; l++) {
              A[offsetA + l * strideA1 + j * strideA2] = 0;
            }
          }
        }
      }
      return 0;
    }
    module.exports = dorglq;
  }
});

// lib/lapack/base/dgebd2/lib/base.js
var require_base62 = __commonJS({
  "lib/lapack/base/dgebd2/lib/base.js"(exports, module) {
    "use strict";
    var dlarfg = require_base20();
    var dlarf = require_base18();
    function dgebd2(M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK) {
      var aii;
      var aij;
      var i;
      if (M === 0 || N === 0) {
        return 0;
      }
      if (M >= N) {
        for (i = 0; i < N; i++) {
          aii = offsetA + i * strideA1 + i * strideA2;
          dlarfg(
            M - i,
            A,
            aii,
            A,
            strideA1,
            offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
            TAUQ,
            offsetTAUQ + i * strideTAUQ
          );
          d[offsetD + i * strideD] = A[aii];
          A[aii] = 1;
          if (i < N - 1) {
            dlarf(
              "left",
              M - i,
              N - i - 1,
              A,
              strideA1,
              aii,
              TAUQ[offsetTAUQ + i * strideTAUQ],
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          A[aii] = d[offsetD + i * strideD];
          if (i < N - 1) {
            aij = offsetA + i * strideA1 + (i + 1) * strideA2;
            dlarfg(
              N - i - 1,
              A,
              aij,
              A,
              strideA2,
              offsetA + i * strideA1 + Math.min(i + 2, N - 1) * strideA2,
              TAUP,
              offsetTAUP + i * strideTAUP
            );
            e[offsetE + i * strideE] = A[aij];
            A[aij] = 1;
            dlarf(
              "right",
              M - i - 1,
              N - i - 1,
              A,
              strideA2,
              aij,
              TAUP[offsetTAUP + i * strideTAUP],
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
            A[aij] = e[offsetE + i * strideE];
          } else {
            TAUP[offsetTAUP + i * strideTAUP] = 0;
          }
        }
      } else {
        for (i = 0; i < M; i++) {
          aii = offsetA + i * strideA1 + i * strideA2;
          dlarfg(
            N - i,
            A,
            aii,
            A,
            strideA2,
            offsetA + i * strideA1 + Math.min(i + 1, N - 1) * strideA2,
            TAUP,
            offsetTAUP + i * strideTAUP
          );
          d[offsetD + i * strideD] = A[aii];
          A[aii] = 1;
          if (i < M - 1) {
            dlarf(
              "right",
              M - i - 1,
              N - i,
              A,
              strideA2,
              aii,
              TAUP[offsetTAUP + i * strideTAUP],
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          A[aii] = d[offsetD + i * strideD];
          if (i < M - 1) {
            aij = offsetA + (i + 1) * strideA1 + i * strideA2;
            dlarfg(
              M - i - 1,
              A,
              aij,
              A,
              strideA1,
              offsetA + Math.min(i + 2, M - 1) * strideA1 + i * strideA2,
              TAUQ,
              offsetTAUQ + i * strideTAUQ
            );
            e[offsetE + i * strideE] = A[aij];
            A[aij] = 1;
            dlarf(
              "left",
              M - i - 1,
              N - i - 1,
              A,
              strideA1,
              aij,
              TAUQ[offsetTAUQ + i * strideTAUQ],
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
            A[aij] = e[offsetE + i * strideE];
          } else {
            TAUQ[offsetTAUQ + i * strideTAUQ] = 0;
          }
        }
      }
      return 0;
    }
    module.exports = dgebd2;
  }
});

// lib/lapack/base/dlabrd/lib/base.js
var require_base63 = __commonJS({
  "lib/lapack/base/dlabrd/lib/base.js"(exports, module) {
    "use strict";
    var dgemv = require_base14();
    var dlarfg = require_base20();
    var dscal = require_base();
    function dlabrd(M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY) {
      var i;
      if (M <= 0 || N <= 0) {
        return;
      }
      if (M >= N) {
        for (i = 0; i < nb; i++) {
          dgemv(
            "no-transpose",
            M - i,
            i,
            -1,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1,
            Y,
            strideY2,
            offsetY + i * strideY1,
            1,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2
          );
          dgemv(
            "no-transpose",
            M - i,
            i,
            -1,
            X,
            strideX1,
            strideX2,
            offsetX + i * strideX1,
            A,
            strideA1,
            offsetA + i * strideA2,
            1,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2
          );
          dlarfg(
            M - i,
            A,
            offsetA + i * strideA1 + i * strideA2,
            A,
            strideA1,
            offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
            TAUQ,
            offsetTAUQ + i * strideTAUQ
          );
          d[offsetD + i * strideD] = A[offsetA + i * strideA1 + i * strideA2];
          if (i < N - 1) {
            A[offsetA + i * strideA1 + i * strideA2] = 1;
            dgemv(
              "transpose",
              M - i,
              N - i - 1,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dgemv(
              "transpose",
              M - i,
              i,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            dgemv(
              "no-transpose",
              N - i - 1,
              i,
              -1,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              Y,
              strideY1,
              offsetY + i * strideY2,
              1,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dgemv(
              "transpose",
              M - i,
              i,
              1,
              X,
              strideX1,
              strideX2,
              offsetX + i * strideX1,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            dgemv(
              "transpose",
              i,
              N - i - 1,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              Y,
              strideY1,
              offsetY + i * strideY2,
              1,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dscal(
              N - i - 1,
              TAUQ[offsetTAUQ + i * strideTAUQ],
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dgemv(
              "no-transpose",
              N - i - 1,
              i + 1,
              -1,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1,
              1,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2
            );
            dgemv(
              "transpose",
              i,
              N - i - 1,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              X,
              strideX2,
              offsetX + i * strideX1,
              1,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2
            );
            dlarfg(
              N - i - 1,
              A,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + Math.min(i + 2, N - 1) * strideA2,
              TAUP,
              offsetTAUP + i * strideTAUP
            );
            e[offsetE + i * strideE] = A[offsetA + i * strideA1 + (i + 1) * strideA2];
            A[offsetA + i * strideA1 + (i + 1) * strideA2] = 1;
            dgemv(
              "no-transpose",
              M - i - 1,
              N - i - 1,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              0,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dgemv(
              "transpose",
              N - i - 1,
              i + 1,
              1,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              0,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i + 1,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              X,
              strideX1,
              offsetX + i * strideX2,
              1,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dgemv(
              "no-transpose",
              i,
              N - i - 1,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              0,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i,
              -1,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              X,
              strideX1,
              offsetX + i * strideX2,
              1,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dscal(
              M - i - 1,
              TAUP[offsetTAUP + i * strideTAUP],
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
          }
        }
      } else {
        for (i = 0; i < nb; i++) {
          dgemv(
            "no-transpose",
            N - i,
            i,
            -1,
            Y,
            strideY1,
            strideY2,
            offsetY + i * strideY1,
            A,
            strideA2,
            offsetA + i * strideA1,
            1,
            A,
            strideA2,
            offsetA + i * strideA1 + i * strideA2
          );
          dgemv(
            "transpose",
            i,
            N - i,
            -1,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA2,
            X,
            strideX2,
            offsetX + i * strideX1,
            1,
            A,
            strideA2,
            offsetA + i * strideA1 + i * strideA2
          );
          dlarfg(
            N - i,
            A,
            offsetA + i * strideA1 + i * strideA2,
            A,
            strideA2,
            offsetA + i * strideA1 + Math.min(i + 1, N - 1) * strideA2,
            TAUP,
            offsetTAUP + i * strideTAUP
          );
          d[offsetD + i * strideD] = A[offsetA + i * strideA1 + i * strideA2];
          if (i < M - 1) {
            A[offsetA + i * strideA1 + i * strideA2] = 1;
            dgemv(
              "no-transpose",
              M - i - 1,
              N - i,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              0,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dgemv(
              "transpose",
              N - i,
              i,
              1,
              Y,
              strideY1,
              strideY2,
              offsetY + i * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              0,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              X,
              strideX1,
              offsetX + i * strideX2,
              1,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dgemv(
              "no-transpose",
              i,
              N - i,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              0,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i,
              -1,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              X,
              strideX1,
              offsetX + i * strideX2,
              1,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dscal(
              M - i - 1,
              TAUP[offsetTAUP + i * strideTAUP],
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              Y,
              strideY2,
              offsetY + i * strideY1,
              1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2
            );
            dgemv(
              "no-transpose",
              M - i - 1,
              i + 1,
              -1,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              A,
              strideA1,
              offsetA + i * strideA2,
              1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2
            );
            dlarfg(
              M - i - 1,
              A,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              A,
              strideA1,
              offsetA + Math.min(i + 2, M - 1) * strideA1 + i * strideA2,
              TAUQ,
              offsetTAUQ + i * strideTAUQ
            );
            e[offsetE + i * strideE] = A[offsetA + (i + 1) * strideA1 + i * strideA2];
            A[offsetA + (i + 1) * strideA1 + i * strideA2] = 1;
            dgemv(
              "transpose",
              M - i - 1,
              N - i - 1,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dgemv(
              "transpose",
              M - i - 1,
              i,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            dgemv(
              "no-transpose",
              N - i - 1,
              i,
              -1,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              Y,
              strideY1,
              offsetY + i * strideY2,
              1,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dgemv(
              "transpose",
              M - i - 1,
              i + 1,
              1,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              0,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            dgemv(
              "transpose",
              i + 1,
              N - i - 1,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              Y,
              strideY1,
              offsetY + i * strideY2,
              1,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            dscal(
              N - i - 1,
              TAUQ[offsetTAUQ + i * strideTAUQ],
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
          }
        }
      }
    }
    module.exports = dlabrd;
  }
});

// lib/lapack/base/dgebrd/lib/base.js
var require_base64 = __commonJS({
  "lib/lapack/base/dgebrd/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dgebd2 = require_base62();
    var dgemm2 = require_base22();
    var dlabrd = require_base63();
    var DEFAULT_NB = 32;
    function dgebrd(M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK, lwork) {
      var ldwrkx;
      var ldwrky;
      var minmn;
      var nbmin;
      var nb;
      var nx;
      var ws;
      var i;
      var j;
      minmn = Math.min(M, N);
      if (minmn === 0) {
        return 0;
      }
      nb = DEFAULT_NB;
      ldwrkx = M;
      ldwrky = N;
      ws = Math.max(M, N);
      nx = minmn;
      if (nb > 1 && nb < minmn) {
        nx = Math.max(nb, 0);
        if (nx < minmn) {
          ws = (M + N) * nb;
          if (lwork < ws) {
            nbmin = 2;
            if (lwork >= (M + N) * nbmin) {
              nb = Math.floor(lwork / (M + N));
            } else {
              nb = 1;
              nx = minmn;
            }
          }
        }
      }
      if (!WORK || WORK.length < ws) {
        WORK = new Float64Array2(ws);
        offsetWORK = 0;
        strideWORK = 1;
      }
      i = 0;
      if (nb >= 2 && nb < minmn && nx < minmn) {
        while (i < minmn - nx) {
          dlabrd(
            M - i,
            N - i,
            nb,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + i * strideA2,
            d,
            strideD,
            offsetD + i * strideD,
            e,
            strideE,
            offsetE + i * strideE,
            TAUQ,
            strideTAUQ,
            offsetTAUQ + i * strideTAUQ,
            TAUP,
            strideTAUP,
            offsetTAUP + i * strideTAUP,
            WORK,
            1,
            ldwrkx,
            offsetWORK,
            WORK,
            1,
            ldwrky,
            offsetWORK + ldwrkx * nb
          );
          if (M - i - nb > 0 && N - i - nb > 0) {
            dgemm2(
              "no-transpose",
              "transpose",
              M - i - nb,
              N - i - nb,
              nb,
              -1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + i * strideA2,
              WORK,
              1,
              ldwrky,
              offsetWORK + ldwrkx * nb + nb,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + (i + nb) * strideA2
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M - i - nb,
              N - i - nb,
              nb,
              -1,
              WORK,
              1,
              ldwrkx,
              offsetWORK + nb,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + nb) * strideA2,
              1,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + (i + nb) * strideA2
            );
          }
          if (M >= N) {
            for (j = i; j < i + nb; j++) {
              A[offsetA + j * strideA1 + j * strideA2] = d[offsetD + j * strideD];
              A[offsetA + j * strideA1 + (j + 1) * strideA2] = e[offsetE + j * strideE];
            }
          } else {
            for (j = i; j < i + nb; j++) {
              A[offsetA + j * strideA1 + j * strideA2] = d[offsetD + j * strideD];
              A[offsetA + (j + 1) * strideA1 + j * strideA2] = e[offsetE + j * strideE];
            }
          }
          i += nb;
        }
      }
      dgebd2(
        M - i,
        N - i,
        A,
        strideA1,
        strideA2,
        offsetA + i * strideA1 + i * strideA2,
        d,
        strideD,
        offsetD + i * strideD,
        e,
        strideE,
        offsetE + i * strideE,
        TAUQ,
        strideTAUQ,
        offsetTAUQ + i * strideTAUQ,
        TAUP,
        strideTAUP,
        offsetTAUP + i * strideTAUP,
        WORK,
        strideWORK,
        offsetWORK
      );
      return 0;
    }
    module.exports = dgebrd;
  }
});

// lib/lapack/base/dorgbr/lib/base.js
var require_base65 = __commonJS({
  "lib/lapack/base/dorgbr/lib/base.js"(exports, module) {
    "use strict";
    var dorgqr = require_base31();
    var dorglq = require_base61();
    function dorgbr(vect, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var wantq;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return 0;
      }
      wantq = vect === "apply-Q";
      if (wantq) {
        if (M >= K) {
          dorgqr(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
        } else {
          for (j = M - 1; j >= 1; j--) {
            A[offsetA + j * strideA2] = 0;
            for (i = j; i < M; i++) {
              A[offsetA + i * strideA1 + j * strideA2] = A[offsetA + i * strideA1 + (j - 1) * strideA2];
            }
          }
          A[offsetA] = 1;
          for (i = 1; i < M; i++) {
            A[offsetA + i * strideA1] = 0;
          }
          if (M > 1) {
            dorgqr(M - 1, M - 1, M - 1, A, strideA1, strideA2, offsetA + strideA1 + strideA2, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
          }
        }
      } else if (K < N) {
        dorglq(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
      } else {
        A[offsetA] = 1;
        for (i = 1; i < N; i++) {
          A[offsetA + i * strideA1] = 0;
        }
        for (j = 1; j < N; j++) {
          for (i = j - 1; i >= 1; i--) {
            A[offsetA + i * strideA1 + j * strideA2] = A[offsetA + (i - 1) * strideA1 + j * strideA2];
          }
          A[offsetA + j * strideA2] = 0;
        }
        if (N > 1) {
          dorglq(N - 1, N - 1, N - 1, A, strideA1, strideA2, offsetA + strideA1 + strideA2, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
        }
      }
      return 0;
    }
    module.exports = dorgbr;
  }
});

// lib/lapack/base/dlas2/lib/base.js
var require_base66 = __commonJS({
  "lib/lapack/base/dlas2/lib/base.js"(exports, module) {
    "use strict";
    function dlas2(f, g, h, out) {
      var fhmn;
      var fhmx;
      var fa;
      var ga;
      var ha;
      var as;
      var at;
      var au;
      var c;
      fa = Math.abs(f);
      ga = Math.abs(g);
      ha = Math.abs(h);
      fhmn = Math.min(fa, ha);
      fhmx = Math.max(fa, ha);
      if (fhmn === 0) {
        out[0] = 0;
        if (fhmx === 0) {
          out[1] = ga;
        } else {
          out[1] = Math.max(fhmx, ga) * Math.sqrt(1 + Math.min(fhmx, ga) / Math.max(fhmx, ga) * (Math.min(fhmx, ga) / Math.max(fhmx, ga)));
        }
      } else if (ga < fhmx) {
        as = 1 + fhmn / fhmx;
        at = (fhmx - fhmn) / fhmx;
        au = ga / fhmx * (ga / fhmx);
        c = 2 / (Math.sqrt(as * as + au) + Math.sqrt(at * at + au));
        out[0] = fhmn * c;
        out[1] = fhmx / c;
      } else {
        au = fhmx / ga;
        if (au === 0) {
          out[0] = fhmn * fhmx / ga;
          out[1] = ga;
        } else {
          as = 1 + fhmn / fhmx;
          at = (fhmx - fhmn) / fhmx;
          c = 1 / (Math.sqrt(1 + as * au * (as * au)) + Math.sqrt(1 + at * au * (at * au)));
          out[0] = fhmn * c * au;
          out[0] += out[0];
          out[1] = ga / (c + c);
        }
      }
      return out;
    }
    module.exports = dlas2;
  }
});

// lib/lapack/base/disnan/lib/base.js
var require_base67 = __commonJS({
  "lib/lapack/base/disnan/lib/base.js"(exports, module) {
    "use strict";
    function disnan(din) {
      return din !== din;
    }
    module.exports = disnan;
  }
});

// lib/lapack/base/dlasq4/lib/base.js
var require_base68 = __commonJS({
  "lib/lapack/base/dlasq4/lib/base.js"(exports, module) {
    "use strict";
    var CNST1 = 0.563;
    var CNST2 = 1.01;
    var CNST3 = 1.05;
    var QURTR = 0.25;
    var THIRD = 0.333;
    var HALF = 0.5;
    var HUNDRD = 100;
    function dlasq4(i0, n0, z, stride, offset, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g) {
      var tau0;
      var gap1;
      var gap2;
      var gam;
      var nn;
      var np;
      var i4;
      var a2;
      var b1;
      var b2;
      var s;
      tau0 = tau;
      function Z(idx) {
        return z[offset + (idx - 1) * stride];
      }
      if (dmin <= 0) {
        tau = -dmin;
        ttype = -1;
        return {
          "tau": tau,
          "ttype": ttype,
          "g": g
        };
      }
      nn = 4 * n0 + pp;
      if (n0in === n0) {
        if (dmin === dn || dmin === dn1) {
          b1 = Math.sqrt(Z(nn - 3)) * Math.sqrt(Z(nn - 5));
          b2 = Math.sqrt(Z(nn - 7)) * Math.sqrt(Z(nn - 9));
          a2 = Z(nn - 7) + Z(nn - 5);
          if (dmin === dn && dmin1 === dn1) {
            gap2 = dmin2 - a2 - dmin2 * QURTR;
            if (gap2 > 0 && gap2 > b2) {
              gap1 = a2 - dn - b2 / gap2 * b2;
            } else {
              gap1 = a2 - dn - (b1 + b2);
            }
            if (gap1 > 0 && gap1 > b1) {
              s = Math.max(dn - b1 / gap1 * b1, HALF * dmin);
              ttype = -2;
            } else {
              s = 0;
              if (dn > b1) {
                s = dn - b1;
              }
              if (a2 > b1 + b2) {
                s = Math.min(s, a2 - (b1 + b2));
              }
              s = Math.max(s, THIRD * dmin);
              ttype = -3;
            }
          } else {
            ttype = -4;
            s = QURTR * dmin;
            if (dmin === dn) {
              gam = dn;
              a2 = 0;
              if (Z(nn - 5) > Z(nn - 7)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b2 = Z(nn - 5) / Z(nn - 7);
              np = nn - 9;
            } else {
              np = nn - 2 * pp;
              gam = dn1;
              if (Z(np - 4) > Z(np - 2)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              a2 = Z(np - 4) / Z(np - 2);
              if (Z(nn - 9) > Z(nn - 11)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b2 = Z(nn - 9) / Z(nn - 11);
              np = nn - 13;
            }
            a2 += b2;
            for (i4 = np; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (b2 === 0) {
                break;
              }
              b1 = b2;
              if (Z(i4) > Z(i4 - 2)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b2 *= Z(i4) / Z(i4 - 2);
              a2 += b2;
              if (HUNDRD * Math.max(b2, b1) < a2 || CNST1 < a2) {
                break;
              }
            }
            a2 *= CNST3;
            if (a2 < CNST1) {
              s = gam * (1 - Math.sqrt(a2)) / (1 + a2);
            }
          }
        } else if (dmin === dn2) {
          ttype = -5;
          s = QURTR * dmin;
          np = nn - 2 * pp;
          b1 = Z(np - 2);
          b2 = Z(np - 6);
          gam = dn2;
          if (Z(np - 8) > b2 || Z(np - 4) > b1) {
            return {
              "tau": tau0,
              "ttype": ttype,
              "g": g
            };
          }
          a2 = Z(np - 8) / b2 * (1 + Z(np - 4) / b1);
          if (n0 - i0 > 2) {
            b2 = Z(nn - 13) / Z(nn - 15);
            a2 += b2;
            for (i4 = nn - 17; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (b2 === 0) {
                break;
              }
              b1 = b2;
              if (Z(i4) > Z(i4 - 2)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b2 *= Z(i4) / Z(i4 - 2);
              a2 += b2;
              if (HUNDRD * Math.max(b2, b1) < a2 || CNST1 < a2) {
                break;
              }
            }
            a2 *= CNST3;
          }
          if (a2 < CNST1) {
            s = gam * (1 - Math.sqrt(a2)) / (1 + a2);
          }
        } else {
          if (ttype === -6) {
            g += THIRD * (1 - g);
          } else if (ttype === -18) {
            g = QURTR * THIRD;
          } else {
            g = QURTR;
          }
          s = g * dmin;
          ttype = -6;
        }
      } else if (n0in === n0 + 1) {
        if (dmin1 === dn1 && dmin2 === dn2) {
          ttype = -7;
          s = THIRD * dmin1;
          if (Z(nn - 5) > Z(nn - 7)) {
            return {
              "tau": tau0,
              "ttype": ttype,
              "g": g
            };
          }
          b1 = Z(nn - 5) / Z(nn - 7);
          b2 = b1;
          if (b2 !== 0) {
            for (i4 = 4 * n0 - 9 + pp; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              a2 = b1;
              if (Z(i4) > Z(i4 - 2)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b1 *= Z(i4) / Z(i4 - 2);
              b2 += b1;
              if (HUNDRD * Math.max(b1, a2) < b2) {
                break;
              }
            }
          }
          b2 = Math.sqrt(CNST3 * b2);
          a2 = dmin1 / (1 + b2 * b2);
          gap2 = HALF * dmin2 - a2;
          if (gap2 > 0 && gap2 > b2 * a2) {
            s = Math.max(s, a2 * (1 - CNST2 * a2 * (b2 / gap2) * b2));
          } else {
            s = Math.max(s, a2 * (1 - CNST2 * b2));
            ttype = -8;
          }
        } else {
          s = QURTR * dmin1;
          if (dmin1 === dn1) {
            s = HALF * dmin1;
          }
          ttype = -9;
        }
      } else if (n0in === n0 + 2) {
        if (dmin2 === dn2 && 2 * Z(nn - 5) < Z(nn - 7)) {
          ttype = -10;
          s = THIRD * dmin2;
          if (Z(nn - 5) > Z(nn - 7)) {
            return {
              "tau": tau0,
              "ttype": ttype,
              "g": g
            };
          }
          b1 = Z(nn - 5) / Z(nn - 7);
          b2 = b1;
          if (b2 !== 0) {
            for (i4 = 4 * n0 - 9 + pp; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (Z(i4) > Z(i4 - 2)) {
                return {
                  "tau": tau0,
                  "ttype": ttype,
                  "g": g
                };
              }
              b1 *= Z(i4) / Z(i4 - 2);
              b2 += b1;
              if (HUNDRD * b1 < b2) {
                break;
              }
            }
          }
          b2 = Math.sqrt(CNST3 * b2);
          a2 = dmin2 / (1 + b2 * b2);
          gap2 = Z(nn - 7) + Z(nn - 9) - Math.sqrt(Z(nn - 11)) * Math.sqrt(Z(nn - 9)) - a2;
          if (gap2 > 0 && gap2 > b2 * a2) {
            s = Math.max(s, a2 * (1 - CNST2 * a2 * (b2 / gap2) * b2));
          } else {
            s = Math.max(s, a2 * (1 - CNST2 * b2));
          }
        } else {
          s = QURTR * dmin2;
          ttype = -11;
        }
      } else if (n0in > n0 + 2) {
        s = 0;
        ttype = -12;
      }
      tau = s;
      return {
        "tau": tau,
        "ttype": ttype,
        "g": g
      };
    }
    module.exports = dlasq4;
  }
});

// lib/lapack/base/dlasq5/lib/base.js
var require_base69 = __commonJS({
  "lib/lapack/base/dlasq5/lib/base.js"(exports, module) {
    "use strict";
    function dlasq5(i0, n0, z, stride, offset, pp, tau, sigma, ieee, eps) {
      var dthresh;
      var dmin1;
      var dmin2;
      var dnm1;
      var dnm2;
      var emin;
      var dmin;
      var temp;
      var j4p2;
      var j4;
      var dn;
      var d;
      if (n0 - i0 - 1 <= 0) {
        return {
          "dmin": 0,
          "dmin1": 0,
          "dmin2": 0,
          "dn": 0,
          "dnm1": 0,
          "dnm2": 0
        };
      }
      dthresh = eps * (sigma + tau);
      if (tau < dthresh * 0.5) {
        tau = 0;
      }
      function Z(k) {
        return z[offset + (k - 1) * stride];
      }
      function setZ(k, val) {
        z[offset + (k - 1) * stride] = val;
      }
      if (tau === 0) {
        j4 = 4 * i0 + pp - 3;
        emin = Z(j4 + 4);
        d = Z(j4) - tau;
        dmin = d;
        dmin1 = -Z(j4);
        if (ieee) {
          if (pp === 0) {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 2, d + Z(j4 - 1));
              temp = Z(j4 + 1) / Z(j4 - 2);
              d = d * temp - tau;
              if (d < dthresh) {
                d = 0;
              }
              dmin = Math.min(dmin, d);
              setZ(j4, Z(j4 - 1) * temp);
              emin = Math.min(Z(j4), emin);
            }
          } else {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 3, d + Z(j4));
              temp = Z(j4 + 2) / Z(j4 - 3);
              d = d * temp - tau;
              if (d < dthresh) {
                d = 0;
              }
              dmin = Math.min(dmin, d);
              setZ(j4 - 1, Z(j4) * temp);
              emin = Math.min(Z(j4 - 1), emin);
            }
          }
          dnm2 = d;
          dmin2 = dmin;
          j4 = 4 * (n0 - 2) - pp;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm2 + Z(j4p2));
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dnm1 = Z(j4p2 + 2) * (dnm2 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dnm1);
          dmin1 = dmin;
          j4 += 4;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm1 + Z(j4p2));
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dn = Z(j4p2 + 2) * (dnm1 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dn);
        } else {
          if (pp === 0) {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 2, d + Z(j4 - 1));
              if (d < 0) {
                return {
                  "dmin": dmin,
                  "dmin1": dmin1,
                  "dmin2": 0,
                  "dn": 0,
                  "dnm1": 0,
                  "dnm2": 0
                };
              }
              setZ(j4, Z(j4 + 1) * (Z(j4 - 1) / Z(j4 - 2)));
              d = Z(j4 + 1) * (d / Z(j4 - 2)) - tau;
              if (d < dthresh) {
                d = 0;
              }
              dmin = Math.min(dmin, d);
              emin = Math.min(emin, Z(j4));
            }
          } else {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 3, d + Z(j4));
              if (d < 0) {
                return {
                  "dmin": dmin,
                  "dmin1": dmin1,
                  "dmin2": 0,
                  "dn": 0,
                  "dnm1": 0,
                  "dnm2": 0
                };
              }
              setZ(j4 - 1, Z(j4 + 2) * (Z(j4) / Z(j4 - 3)));
              d = Z(j4 + 2) * (d / Z(j4 - 3)) - tau;
              if (d < dthresh) {
                d = 0;
              }
              dmin = Math.min(dmin, d);
              emin = Math.min(emin, Z(j4 - 1));
            }
          }
          dnm2 = d;
          dmin2 = dmin;
          j4 = 4 * (n0 - 2) - pp;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm2 + Z(j4p2));
          if (dnm2 < 0) {
            return {
              "dmin": dmin,
              "dmin1": dmin1,
              "dmin2": dmin2,
              "dn": 0,
              "dnm1": 0,
              "dnm2": dnm2
            };
          }
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dnm1 = Z(j4p2 + 2) * (dnm2 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dnm1);
          dmin1 = dmin;
          j4 += 4;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm1 + Z(j4p2));
          if (dnm1 < 0) {
            return {
              "dmin": dmin,
              "dmin1": dmin1,
              "dmin2": dmin2,
              "dn": 0,
              "dnm1": dnm1,
              "dnm2": dnm2
            };
          }
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dn = Z(j4p2 + 2) * (dnm1 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dn);
        }
      } else {
        j4 = 4 * i0 + pp - 3;
        emin = Z(j4 + 4);
        d = Z(j4) - tau;
        dmin = d;
        dmin1 = -Z(j4);
        if (ieee) {
          if (pp === 0) {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 2, d + Z(j4 - 1));
              temp = Z(j4 + 1) / Z(j4 - 2);
              d = d * temp - tau;
              dmin = Math.min(dmin, d);
              setZ(j4, Z(j4 - 1) * temp);
              emin = Math.min(Z(j4), emin);
            }
          } else {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 3, d + Z(j4));
              temp = Z(j4 + 2) / Z(j4 - 3);
              d = d * temp - tau;
              dmin = Math.min(dmin, d);
              setZ(j4 - 1, Z(j4) * temp);
              emin = Math.min(Z(j4 - 1), emin);
            }
          }
          dnm2 = d;
          dmin2 = dmin;
          j4 = 4 * (n0 - 2) - pp;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm2 + Z(j4p2));
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dnm1 = Z(j4p2 + 2) * (dnm2 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dnm1);
          dmin1 = dmin;
          j4 += 4;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm1 + Z(j4p2));
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dn = Z(j4p2 + 2) * (dnm1 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dn);
        } else {
          if (pp === 0) {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 2, d + Z(j4 - 1));
              if (d < 0) {
                return {
                  "dmin": dmin,
                  "dmin1": dmin1,
                  "dmin2": 0,
                  "dn": 0,
                  "dnm1": 0,
                  "dnm2": 0
                };
              }
              setZ(j4, Z(j4 + 1) * (Z(j4 - 1) / Z(j4 - 2)));
              d = Z(j4 + 1) * (d / Z(j4 - 2)) - tau;
              dmin = Math.min(dmin, d);
              emin = Math.min(emin, Z(j4));
            }
          } else {
            for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
              setZ(j4 - 3, d + Z(j4));
              if (d < 0) {
                return {
                  "dmin": dmin,
                  "dmin1": dmin1,
                  "dmin2": 0,
                  "dn": 0,
                  "dnm1": 0,
                  "dnm2": 0
                };
              }
              setZ(j4 - 1, Z(j4 + 2) * (Z(j4) / Z(j4 - 3)));
              d = Z(j4 + 2) * (d / Z(j4 - 3)) - tau;
              dmin = Math.min(dmin, d);
              emin = Math.min(emin, Z(j4 - 1));
            }
          }
          dnm2 = d;
          dmin2 = dmin;
          j4 = 4 * (n0 - 2) - pp;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm2 + Z(j4p2));
          if (dnm2 < 0) {
            return {
              "dmin": dmin,
              "dmin1": dmin1,
              "dmin2": dmin2,
              "dn": 0,
              "dnm1": 0,
              "dnm2": dnm2
            };
          }
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dnm1 = Z(j4p2 + 2) * (dnm2 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dnm1);
          dmin1 = dmin;
          j4 += 4;
          j4p2 = j4 + 2 * pp - 1;
          setZ(j4 - 2, dnm1 + Z(j4p2));
          if (dnm1 < 0) {
            return {
              "dmin": dmin,
              "dmin1": dmin1,
              "dmin2": dmin2,
              "dn": 0,
              "dnm1": dnm1,
              "dnm2": dnm2
            };
          }
          setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
          dn = Z(j4p2 + 2) * (dnm1 / Z(j4 - 2)) - tau;
          dmin = Math.min(dmin, dn);
        }
      }
      setZ(j4 + 2, dn);
      setZ(4 * n0 - pp, emin);
      return {
        "dmin": dmin,
        "dmin1": dmin1,
        "dmin2": dmin2,
        "dn": dn,
        "dnm1": dnm1,
        "dnm2": dnm2
      };
    }
    module.exports = dlasq5;
  }
});

// lib/lapack/base/dlasq6/lib/base.js
var require_base70 = __commonJS({
  "lib/lapack/base/dlasq6/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    function dlasq6(i0, n0, z, stride, offset, pp) {
      var safmin;
      var dmin1;
      var dmin2;
      var dnm1;
      var dnm2;
      var emin;
      var dmin;
      var temp;
      var j4p2;
      var j4;
      var dn;
      var d;
      if (n0 - i0 - 1 <= 0) {
        return {
          "dmin": 0,
          "dmin1": 0,
          "dmin2": 0,
          "dn": 0,
          "dnm1": 0,
          "dnm2": 0
        };
      }
      safmin = dlamch("Safe minimum");
      function Z(k) {
        return z[offset + (k - 1) * stride];
      }
      function setZ(k, val) {
        z[offset + (k - 1) * stride] = val;
      }
      j4 = 4 * i0 + pp - 3;
      emin = Z(j4 + 4);
      d = Z(j4);
      dmin = d;
      if (pp === 0) {
        for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
          setZ(j4 - 2, d + Z(j4 - 1));
          if (Z(j4 - 2) === 0) {
            setZ(j4, 0);
            d = Z(j4 + 1);
            dmin = d;
            emin = 0;
          } else if (safmin * Z(j4 + 1) < Z(j4 - 2) && safmin * Z(j4 - 2) < Z(j4 + 1)) {
            temp = Z(j4 + 1) / Z(j4 - 2);
            setZ(j4, Z(j4 - 1) * temp);
            d *= temp;
          } else {
            setZ(j4, Z(j4 + 1) * (Z(j4 - 1) / Z(j4 - 2)));
            d = Z(j4 + 1) * (d / Z(j4 - 2));
          }
          dmin = Math.min(dmin, d);
          emin = Math.min(emin, Z(j4));
        }
      } else {
        for (j4 = 4 * i0; j4 <= 4 * (n0 - 3); j4 += 4) {
          setZ(j4 - 3, d + Z(j4));
          if (Z(j4 - 3) === 0) {
            setZ(j4 - 1, 0);
            d = Z(j4 + 2);
            dmin = d;
            emin = 0;
          } else if (safmin * Z(j4 + 2) < Z(j4 - 3) && safmin * Z(j4 - 3) < Z(j4 + 2)) {
            temp = Z(j4 + 2) / Z(j4 - 3);
            setZ(j4 - 1, Z(j4) * temp);
            d *= temp;
          } else {
            setZ(j4 - 1, Z(j4 + 2) * (Z(j4) / Z(j4 - 3)));
            d = Z(j4 + 2) * (d / Z(j4 - 3));
          }
          dmin = Math.min(dmin, d);
          emin = Math.min(emin, Z(j4 - 1));
        }
      }
      dnm2 = d;
      dmin2 = dmin;
      j4 = 4 * (n0 - 2) - pp;
      j4p2 = j4 + 2 * pp - 1;
      setZ(j4 - 2, dnm2 + Z(j4p2));
      if (Z(j4 - 2) === 0) {
        setZ(j4, 0);
        dnm1 = Z(j4p2 + 2);
        dmin = dnm1;
        emin = 0;
      } else if (safmin * Z(j4p2 + 2) < Z(j4 - 2) && safmin * Z(j4 - 2) < Z(j4p2 + 2)) {
        temp = Z(j4p2 + 2) / Z(j4 - 2);
        setZ(j4, Z(j4p2) * temp);
        dnm1 = dnm2 * temp;
      } else {
        setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
        dnm1 = Z(j4p2 + 2) * (dnm2 / Z(j4 - 2));
      }
      dmin = Math.min(dmin, dnm1);
      dmin1 = dmin;
      j4 += 4;
      j4p2 = j4 + 2 * pp - 1;
      setZ(j4 - 2, dnm1 + Z(j4p2));
      if (Z(j4 - 2) === 0) {
        setZ(j4, 0);
        dn = Z(j4p2 + 2);
        dmin = dn;
        emin = 0;
      } else if (safmin * Z(j4p2 + 2) < Z(j4 - 2) && safmin * Z(j4 - 2) < Z(j4p2 + 2)) {
        temp = Z(j4p2 + 2) / Z(j4 - 2);
        setZ(j4, Z(j4p2) * temp);
        dn = dnm1 * temp;
      } else {
        setZ(j4, Z(j4p2 + 2) * (Z(j4p2) / Z(j4 - 2)));
        dn = Z(j4p2 + 2) * (dnm1 / Z(j4 - 2));
      }
      dmin = Math.min(dmin, dn);
      setZ(j4 + 2, dn);
      setZ(4 * n0 - pp, emin);
      return {
        "dmin": dmin,
        "dmin1": dmin1,
        "dmin2": dmin2,
        "dn": dn,
        "dnm1": dnm1,
        "dnm2": dnm2
      };
    }
    module.exports = dlasq6;
  }
});

// lib/lapack/base/dlasq3/lib/base.js
var require_base71 = __commonJS({
  "lib/lapack/base/dlasq3/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var disnan = require_base67();
    var dlasq4 = require_base68();
    var dlasq5 = require_base69();
    var dlasq6 = require_base70();
    var CBIAS = 1.5;
    var QURTR = 0.25;
    var HALF = 0.5;
    var ONE = 1;
    var TWO = 2;
    var HUNDRD = 100;
    var ZERO = 0;
    function dlasq3(i0, n0, z, stride, offset, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau) {
      var n0in;
      var tol2;
      var ipn4;
      var temp;
      var eps;
      var tol;
      var nn;
      var j4;
      var s;
      var t;
      var r;
      function Z(idx) {
        return z[offset + (idx - 1) * stride];
      }
      function setZ(idx, val) {
        z[offset + (idx - 1) * stride] = val;
      }
      n0in = n0;
      eps = dlamch("Precision");
      tol = eps * HUNDRD;
      tol2 = tol * tol;
      while (true) {
        if (n0 < i0) {
          return {
            "n0": n0,
            "pp": pp,
            "dmin": dmin,
            "sigma": sigma,
            "desig": desig,
            "qmax": qmax,
            "nfail": nfail,
            "iter": iter,
            "ndiv": ndiv,
            "ttype": ttype,
            "dmin1": dmin1,
            "dmin2": dmin2,
            "dn": dn,
            "dn1": dn1,
            "dn2": dn2,
            "g": g,
            "tau": tau
          };
        }
        if (n0 === i0) {
          setZ(4 * n0 - 3, Z(4 * n0 + pp - 3) + sigma);
          n0 -= 1;
          continue;
        }
        nn = 4 * n0 + pp;
        if (n0 === i0 + 1) {
        } else if (Z(nn - 5) > tol2 * (sigma + Z(nn - 3)) && Z(nn - 2 * pp - 4) > tol2 * Z(nn - 7)) {
          if (Z(nn - 9) > tol2 * sigma && Z(nn - 2 * pp - 8) > tol2 * Z(nn - 11)) {
            break;
          }
        } else {
          setZ(4 * n0 - 3, Z(4 * n0 + pp - 3) + sigma);
          n0 -= 1;
          continue;
        }
        if (Z(nn - 3) > Z(nn - 7)) {
          s = Z(nn - 3);
          setZ(nn - 3, Z(nn - 7));
          setZ(nn - 7, s);
        }
        t = HALF * (Z(nn - 7) - Z(nn - 3) + Z(nn - 5));
        if (Z(nn - 5) > Z(nn - 3) * tol2 && t !== ZERO) {
          s = Z(nn - 3) * (Z(nn - 5) / t);
          if (s <= t) {
            s = Z(nn - 3) * (Z(nn - 5) / (t * (ONE + Math.sqrt(ONE + s / t))));
          } else {
            s = Z(nn - 3) * (Z(nn - 5) / (t + Math.sqrt(t) * Math.sqrt(t + s)));
          }
          t = Z(nn - 7) + (s + Z(nn - 5));
          setZ(nn - 3, Z(nn - 3) * (Z(nn - 7) / t));
          setZ(nn - 7, t);
        }
        setZ(4 * n0 - 7, Z(nn - 7) + sigma);
        setZ(4 * n0 - 3, Z(nn - 3) + sigma);
        n0 -= 2;
        continue;
      }
      if (pp === 2) {
        pp = 0;
      }
      if (dmin <= ZERO || n0 < n0in) {
        if (CBIAS * Z(4 * i0 + pp - 3) < Z(4 * n0 + pp - 3)) {
          ipn4 = 4 * (i0 + n0);
          for (j4 = 4 * i0; j4 <= 2 * (i0 + n0 - 1); j4 += 4) {
            temp = Z(j4 - 3);
            setZ(j4 - 3, Z(ipn4 - j4 - 3));
            setZ(ipn4 - j4 - 3, temp);
            temp = Z(j4 - 2);
            setZ(j4 - 2, Z(ipn4 - j4 - 2));
            setZ(ipn4 - j4 - 2, temp);
            temp = Z(j4 - 1);
            setZ(j4 - 1, Z(ipn4 - j4 - 5));
            setZ(ipn4 - j4 - 5, temp);
            temp = Z(j4);
            setZ(j4, Z(ipn4 - j4 - 4));
            setZ(ipn4 - j4 - 4, temp);
          }
          if (n0 - i0 <= 4) {
            setZ(4 * n0 + pp - 1, Z(4 * i0 + pp - 1));
            setZ(4 * n0 - pp, Z(4 * i0 - pp));
          }
          dmin2 = Math.min(dmin2, Z(4 * n0 + pp - 1));
          setZ(4 * n0 + pp - 1, Math.min(
            Z(4 * n0 + pp - 1),
            Z(4 * i0 + pp - 1),
            Z(4 * i0 + pp + 3)
          ));
          setZ(4 * n0 - pp, Math.min(
            Z(4 * n0 - pp),
            Z(4 * i0 - pp),
            Z(4 * i0 - pp + 4)
          ));
          qmax = Math.max(qmax, Z(4 * i0 + pp - 3), Z(4 * i0 + pp + 1));
          dmin = -ZERO;
        }
      }
      r = dlasq4(
        i0,
        n0,
        z,
        stride,
        offset,
        pp,
        n0in,
        dmin,
        dmin1,
        dmin2,
        dn,
        dn1,
        dn2,
        tau,
        ttype,
        g
      );
      tau = r.tau;
      ttype = r.ttype;
      g = r.g;
      while (true) {
        r = dlasq5(i0, n0, z, stride, offset, pp, tau, sigma, ieee, eps);
        dmin = r.dmin;
        dmin1 = r.dmin1;
        dmin2 = r.dmin2;
        dn = r.dn;
        dn1 = r.dnm1;
        dn2 = r.dnm2;
        ndiv += n0 - i0 + 2;
        iter += 1;
        if (dmin >= ZERO && dmin1 >= ZERO) {
          break;
        } else if (dmin < ZERO && dmin1 > ZERO && Z(4 * (n0 - 1) - pp) < tol * (sigma + dn1) && Math.abs(dn) < tol * sigma) {
          setZ(4 * (n0 - 1) - pp + 2, ZERO);
          dmin = ZERO;
          break;
        } else if (dmin < ZERO) {
          nfail += 1;
          if (ttype < -22) {
            tau = ZERO;
          } else if (dmin1 > ZERO) {
            tau = (tau + dmin) * (ONE - TWO * eps);
            ttype -= 11;
          } else {
            tau *= QURTR;
            ttype -= 12;
          }
          continue;
        } else if (disnan(dmin)) {
          if (tau === ZERO) {
            r = dlasq6(i0, n0, z, stride, offset, pp);
            dmin = r.dmin;
            dmin1 = r.dmin1;
            dmin2 = r.dmin2;
            dn = r.dn;
            dn1 = r.dnm1;
            dn2 = r.dnm2;
            ndiv += n0 - i0 + 2;
            iter += 1;
            tau = ZERO;
            break;
          }
          tau = ZERO;
          continue;
        } else {
          r = dlasq6(i0, n0, z, stride, offset, pp);
          dmin = r.dmin;
          dmin1 = r.dmin1;
          dmin2 = r.dmin2;
          dn = r.dn;
          dn1 = r.dnm1;
          dn2 = r.dnm2;
          ndiv += n0 - i0 + 2;
          iter += 1;
          tau = ZERO;
          break;
        }
      }
      if (tau < sigma) {
        desig += tau;
        t = sigma + desig;
        desig -= t - sigma;
      } else {
        t = sigma + tau;
        desig = sigma - (t - tau) + desig;
      }
      sigma = t;
      return {
        "n0": n0,
        "pp": pp,
        "dmin": dmin,
        "sigma": sigma,
        "desig": desig,
        "qmax": qmax,
        "nfail": nfail,
        "iter": iter,
        "ndiv": ndiv,
        "ttype": ttype,
        "dmin1": dmin1,
        "dmin2": dmin2,
        "dn": dn,
        "dn1": dn1,
        "dn2": dn2,
        "g": g,
        "tau": tau
      };
    }
    module.exports = dlasq3;
  }
});

// lib/lapack/base/dlasrt/lib/base.js
var require_base72 = __commonJS({
  "lib/lapack/base/dlasrt/lib/base.js"(exports, module) {
    "use strict";
    var SELECT = 20;
    function dlasrt(id, N, d, stride, offset) {
      var stkpnt;
      var stack;
      var start;
      var dmnmx;
      var endd;
      var dir;
      var tmp;
      var d1;
      var d2;
      var d3;
      var i;
      var j;
      dir = -1;
      if (id === "decreasing") {
        dir = 0;
      } else if (id === "increasing") {
        dir = 1;
      }
      if (dir === -1) {
        return -1;
      }
      if (N < 0) {
        return -2;
      }
      if (N <= 1) {
        return 0;
      }
      stkpnt = 1;
      stack = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
      stack[0] = 0;
      stack[1] = N - 1;
      while (stkpnt > 0) {
        stkpnt -= 1;
        start = stack[2 * stkpnt];
        endd = stack[2 * stkpnt + 1];
        if (endd - start <= SELECT && endd - start > 0) {
          if (dir === 0) {
            for (i = start + 1; i <= endd; i++) {
              for (j = i; j >= start + 1; j--) {
                if (d[offset + j * stride] > d[offset + (j - 1) * stride]) {
                  dmnmx = d[offset + j * stride];
                  d[offset + j * stride] = d[offset + (j - 1) * stride];
                  d[offset + (j - 1) * stride] = dmnmx;
                } else {
                  break;
                }
              }
            }
          } else {
            for (i = start + 1; i <= endd; i++) {
              for (j = i; j >= start + 1; j--) {
                if (d[offset + j * stride] < d[offset + (j - 1) * stride]) {
                  dmnmx = d[offset + j * stride];
                  d[offset + j * stride] = d[offset + (j - 1) * stride];
                  d[offset + (j - 1) * stride] = dmnmx;
                } else {
                  break;
                }
              }
            }
          }
        } else if (endd - start > SELECT) {
          d1 = d[offset + start * stride];
          d2 = d[offset + endd * stride];
          i = (start + endd) / 2 | 0;
          d3 = d[offset + i * stride];
          if (d1 < d2) {
            if (d3 < d1) {
              dmnmx = d1;
            } else if (d3 < d2) {
              dmnmx = d3;
            } else {
              dmnmx = d2;
            }
          } else if (d3 < d2) {
            dmnmx = d2;
          } else if (d3 < d1) {
            dmnmx = d3;
          } else {
            dmnmx = d1;
          }
          if (dir === 0) {
            i = start - 1;
            j = endd + 1;
            while (true) {
              do {
                j -= 1;
              } while (d[offset + j * stride] < dmnmx);
              do {
                i += 1;
              } while (d[offset + i * stride] > dmnmx);
              if (i < j) {
                tmp = d[offset + i * stride];
                d[offset + i * stride] = d[offset + j * stride];
                d[offset + j * stride] = tmp;
              } else {
                break;
              }
            }
          } else {
            i = start - 1;
            j = endd + 1;
            while (true) {
              do {
                j -= 1;
              } while (d[offset + j * stride] > dmnmx);
              do {
                i += 1;
              } while (d[offset + i * stride] < dmnmx);
              if (i < j) {
                tmp = d[offset + i * stride];
                d[offset + i * stride] = d[offset + j * stride];
                d[offset + j * stride] = tmp;
              } else {
                break;
              }
            }
          }
          if (j - start > endd - j - 1) {
            stack[2 * stkpnt] = start;
            stack[2 * stkpnt + 1] = j;
            stkpnt += 1;
            stack[2 * stkpnt] = j + 1;
            stack[2 * stkpnt + 1] = endd;
            stkpnt += 1;
          } else {
            stack[2 * stkpnt] = j + 1;
            stack[2 * stkpnt + 1] = endd;
            stkpnt += 1;
            stack[2 * stkpnt] = start;
            stack[2 * stkpnt + 1] = j;
            stkpnt += 1;
          }
        }
      }
      return 0;
    }
    module.exports = dlasrt;
  }
});

// lib/lapack/base/dlasq2/lib/base.js
var require_base73 = __commonJS({
  "lib/lapack/base/dlasq2/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base5();
    var dlasq3 = require_base71();
    var dlasrt = require_base72();
    var CBIAS = 1.5;
    var ZERO = 0;
    var HALF = 0.5;
    var ONE = 1;
    var TWO = 2;
    var FOUR = 4;
    var HUNDRD = 100;
    function dlasq2(N, z, stride, offset) {
      var deemin;
      var oldemn;
      var safmin;
      var iwhila;
      var iwhilb;
      var desig;
      var dmin1;
      var dmin2;
      var sigma;
      var trace;
      var tempq;
      var tempe;
      var ttype;
      var nfail;
      var emax;
      var emin;
      var info;
      var iter;
      var nbig;
      var ndiv;
      var ipn4;
      var kmin;
      var dmin;
      var qmax;
      var qmin;
      var temp;
      var tol2;
      var zmax;
      var splt;
      var dee;
      var eps;
      var tol;
      var tau;
      var dn1;
      var dn2;
      var pp;
      var i0;
      var i1;
      var i4;
      var n0;
      var n1;
      var dn;
      var r;
      var d;
      var e;
      var g;
      var k;
      var s;
      var t;
      function Z(idx) {
        return z[offset + (idx - 1) * stride];
      }
      function setZ(idx, val) {
        z[offset + (idx - 1) * stride] = val;
      }
      info = 0;
      eps = dlamch("Precision");
      safmin = dlamch("Safe minimum");
      tol = eps * HUNDRD;
      tol2 = tol * tol;
      if (N < 0) {
        return -1;
      }
      if (N === 0) {
        return 0;
      }
      if (N === 1) {
        if (Z(1) < ZERO) {
          return -201;
        }
        return 0;
      }
      if (N === 2) {
        if (Z(1) < ZERO) {
          return -201;
        }
        if (Z(2) < ZERO) {
          return -202;
        }
        if (Z(3) < ZERO) {
          return -203;
        }
        if (Z(3) > Z(1)) {
          d = Z(3);
          setZ(3, Z(1));
          setZ(1, d);
        }
        setZ(5, Z(1) + Z(2) + Z(3));
        if (Z(2) > Z(3) * tol2) {
          t = HALF * (Z(1) - Z(3) + Z(2));
          s = Z(3) * (Z(2) / t);
          if (s <= t) {
            s = Z(3) * (Z(2) / (t * (ONE + Math.sqrt(ONE + s / t))));
          } else {
            s = Z(3) * (Z(2) / (t + Math.sqrt(t) * Math.sqrt(t + s)));
          }
          t = Z(1) + (s + Z(2));
          setZ(3, Z(3) * (Z(1) / t));
          setZ(1, t);
        }
        setZ(2, Z(3));
        setZ(6, Z(2) + Z(1));
        return 0;
      }
      setZ(2 * N, ZERO);
      emin = Z(2);
      qmax = ZERO;
      zmax = ZERO;
      d = ZERO;
      e = ZERO;
      for (k = 1; k <= 2 * (N - 1); k += 2) {
        if (Z(k) < ZERO) {
          return -(200 + k);
        }
        if (Z(k + 1) < ZERO) {
          return -(200 + k + 1);
        }
        d += Z(k);
        e += Z(k + 1);
        qmax = Math.max(qmax, Z(k));
        emin = Math.min(emin, Z(k + 1));
        zmax = Math.max(qmax, zmax, Z(k + 1));
      }
      if (Z(2 * N - 1) < ZERO) {
        return -(200 + 2 * N - 1);
      }
      d += Z(2 * N - 1);
      qmax = Math.max(qmax, Z(2 * N - 1));
      zmax = Math.max(qmax, zmax);
      if (e === ZERO) {
        for (k = 2; k <= N; k++) {
          setZ(k, Z(2 * k - 1));
        }
        dlasrt("decreasing", N, z, stride, offset);
        setZ(2 * N - 1, d);
        return 0;
      }
      trace = d + e;
      if (trace === ZERO) {
        setZ(2 * N - 1, ZERO);
        return 0;
      }
      for (k = 2 * N; k >= 2; k -= 2) {
        setZ(2 * k, ZERO);
        setZ(2 * k - 1, Z(k));
        setZ(2 * k - 2, ZERO);
        setZ(2 * k - 3, Z(k - 1));
      }
      i0 = 1;
      n0 = N;
      if (CBIAS * Z(4 * i0 - 3) < Z(4 * n0 - 3)) {
        ipn4 = 4 * (i0 + n0);
        for (i4 = 4 * i0; i4 <= 2 * (i0 + n0 - 1); i4 += 4) {
          temp = Z(i4 - 3);
          setZ(i4 - 3, Z(ipn4 - i4 - 3));
          setZ(ipn4 - i4 - 3, temp);
          temp = Z(i4 - 1);
          setZ(i4 - 1, Z(ipn4 - i4 - 5));
          setZ(ipn4 - i4 - 5, temp);
        }
      }
      pp = 0;
      for (k = 1; k <= 2; k++) {
        d = Z(4 * n0 + pp - 3);
        for (i4 = 4 * (n0 - 1) + pp; i4 >= 4 * i0 + pp; i4 -= 4) {
          if (Z(i4 - 1) <= tol2 * d) {
            setZ(i4 - 1, -ZERO);
            d = Z(i4 - 3);
          } else {
            d = Z(i4 - 3) * (d / (d + Z(i4 - 1)));
          }
        }
        emin = Z(4 * i0 + pp + 1);
        d = Z(4 * i0 + pp - 3);
        for (i4 = 4 * i0 + pp; i4 <= 4 * (n0 - 1) + pp; i4 += 4) {
          setZ(i4 - 2 * pp - 2, d + Z(i4 - 1));
          if (Z(i4 - 1) <= tol2 * d) {
            setZ(i4 - 1, -ZERO);
            setZ(i4 - 2 * pp - 2, d);
            setZ(i4 - 2 * pp, ZERO);
            d = Z(i4 + 1);
          } else if (safmin * Z(i4 + 1) < Z(i4 - 2 * pp - 2) && safmin * Z(i4 - 2 * pp - 2) < Z(i4 + 1)) {
            temp = Z(i4 + 1) / Z(i4 - 2 * pp - 2);
            setZ(i4 - 2 * pp, Z(i4 - 1) * temp);
            d *= temp;
          } else {
            setZ(i4 - 2 * pp, Z(i4 + 1) * (Z(i4 - 1) / Z(i4 - 2 * pp - 2)));
            d = Z(i4 + 1) * (d / Z(i4 - 2 * pp - 2));
          }
          emin = Math.min(emin, Z(i4 - 2 * pp));
        }
        setZ(4 * n0 - pp - 2, d);
        qmax = Z(4 * i0 - pp - 2);
        for (i4 = 4 * i0 - pp + 2; i4 <= 4 * n0 - pp - 2; i4 += 4) {
          qmax = Math.max(qmax, Z(i4));
        }
        pp = 1 - pp;
      }
      ttype = 0;
      dmin1 = ZERO;
      dmin2 = ZERO;
      dn = ZERO;
      dn1 = ZERO;
      dn2 = ZERO;
      g = ZERO;
      tau = ZERO;
      iter = 2;
      nfail = 0;
      ndiv = 2 * (n0 - i0);
      for (iwhila = 1; iwhila <= N + 1; iwhila++) {
        if (n0 < 1) {
          for (k = 2; k <= N; k++) {
            setZ(k, Z(4 * k - 3));
          }
          dlasrt("decreasing", N, z, stride, offset);
          e = ZERO;
          for (k = N; k >= 1; k--) {
            e += Z(k);
          }
          setZ(2 * N + 1, trace);
          setZ(2 * N + 2, e);
          setZ(2 * N + 3, iter);
          setZ(2 * N + 4, ndiv / (N * N));
          setZ(2 * N + 5, HUNDRD * nfail / iter);
          return 0;
        }
        desig = ZERO;
        if (n0 === N) {
          sigma = ZERO;
        } else {
          sigma = -Z(4 * n0 - 1);
        }
        if (sigma < ZERO) {
          info = 1;
          return info;
        }
        emax = ZERO;
        if (n0 > i0) {
          emin = Math.abs(Z(4 * n0 - 5));
        } else {
          emin = ZERO;
        }
        qmin = Z(4 * n0 - 3);
        qmax = qmin;
        for (i4 = 4 * n0; i4 >= 8; i4 -= 4) {
          if (Z(i4 - 5) <= ZERO) {
            break;
          }
          if (qmin >= FOUR * emax) {
            qmin = Math.min(qmin, Z(i4 - 3));
            emax = Math.max(emax, Z(i4 - 5));
          }
          qmax = Math.max(qmax, Z(i4 - 7) + Z(i4 - 5));
          emin = Math.min(emin, Z(i4 - 5));
        }
        if (i4 < 8) {
          i4 = 4;
        }
        i0 = i4 / 4;
        pp = 0;
        if (n0 - i0 > 1) {
          dee = Z(4 * i0 - 3);
          deemin = dee;
          kmin = i0;
          for (i4 = 4 * i0 + 1; i4 <= 4 * n0 - 3; i4 += 4) {
            dee = Z(i4) * (dee / (dee + Z(i4 - 2)));
            if (dee <= deemin) {
              deemin = dee;
              kmin = (i4 + 3) / 4 | 0;
            }
          }
          if ((kmin - i0) * 2 < n0 - kmin && deemin <= HALF * Z(4 * n0 - 3)) {
            ipn4 = 4 * (i0 + n0);
            pp = 2;
            for (i4 = 4 * i0; i4 <= 2 * (i0 + n0 - 1); i4 += 4) {
              temp = Z(i4 - 3);
              setZ(i4 - 3, Z(ipn4 - i4 - 3));
              setZ(ipn4 - i4 - 3, temp);
              temp = Z(i4 - 2);
              setZ(i4 - 2, Z(ipn4 - i4 - 2));
              setZ(ipn4 - i4 - 2, temp);
              temp = Z(i4 - 1);
              setZ(i4 - 1, Z(ipn4 - i4 - 5));
              setZ(ipn4 - i4 - 5, temp);
              temp = Z(i4);
              setZ(i4, Z(ipn4 - i4 - 4));
              setZ(ipn4 - i4 - 4, temp);
            }
          }
        }
        dmin = -Math.max(ZERO, qmin - TWO * Math.sqrt(qmin) * Math.sqrt(emax));
        nbig = 100 * (n0 - i0 + 1);
        for (iwhilb = 1; iwhilb <= nbig; iwhilb++) {
          if (i0 > n0) {
            break;
          }
          r = dlasq3(
            i0,
            n0,
            z,
            stride,
            offset,
            pp,
            dmin,
            sigma,
            desig,
            qmax,
            nfail,
            iter,
            ndiv,
            true,
            ttype,
            dmin1,
            dmin2,
            dn,
            dn1,
            dn2,
            g,
            tau
          );
          n0 = r.n0;
          pp = r.pp;
          dmin = r.dmin;
          sigma = r.sigma;
          desig = r.desig;
          qmax = r.qmax;
          nfail = r.nfail;
          iter = r.iter;
          ndiv = r.ndiv;
          ttype = r.ttype;
          dmin1 = r.dmin1;
          dmin2 = r.dmin2;
          dn = r.dn;
          dn1 = r.dn1;
          dn2 = r.dn2;
          g = r.g;
          tau = r.tau;
          pp = 1 - pp;
          if (pp === 0 && n0 - i0 >= 3) {
            if (Z(4 * n0) <= tol2 * qmax || Z(4 * n0 - 1) <= tol2 * sigma) {
              splt = i0 - 1;
              qmax = Z(4 * i0 - 3);
              emin = Z(4 * i0 - 1);
              oldemn = Z(4 * i0);
              for (i4 = 4 * i0; i4 <= 4 * (n0 - 3); i4 += 4) {
                if (Z(i4) <= tol2 * Z(i4 - 3) || Z(i4 - 1) <= tol2 * sigma) {
                  setZ(i4 - 1, -sigma);
                  splt = i4 / 4 | 0;
                  qmax = ZERO;
                  emin = Z(i4 + 3);
                  oldemn = Z(i4 + 4);
                } else {
                  qmax = Math.max(qmax, Z(i4 + 1));
                  emin = Math.min(emin, Z(i4 - 1));
                  oldemn = Math.min(oldemn, Z(i4));
                }
              }
              setZ(4 * n0 - 1, emin);
              setZ(4 * n0, oldemn);
              i0 = splt + 1;
            }
          }
        }
        if (iwhilb > nbig) {
          info = 2;
          i1 = i0;
          n1 = n0;
          while (true) {
            tempq = Z(4 * i0 - 3);
            setZ(4 * i0 - 3, Z(4 * i0 - 3) + sigma);
            for (k = i0 + 1; k <= n0; k++) {
              tempe = Z(4 * k - 5);
              setZ(4 * k - 5, Z(4 * k - 5) * (tempq / Z(4 * k - 7)));
              tempq = Z(4 * k - 3);
              setZ(4 * k - 3, Z(4 * k - 3) + sigma + tempe - Z(4 * k - 5));
            }
            if (i1 > 1) {
              n1 = i1 - 1;
              while (i1 >= 2 && Z(4 * i1 - 5) >= ZERO) {
                i1 -= 1;
              }
              sigma = -Z(4 * n1 - 1);
              i0 = i1;
            } else {
              break;
            }
          }
          for (k = 1; k <= N; k++) {
            setZ(2 * k - 1, Z(4 * k - 3));
            if (k < n0) {
              setZ(2 * k, Z(4 * k - 1));
            } else {
              setZ(2 * k, 0);
            }
          }
          return info;
        }
      }
      return 3;
    }
    module.exports = dlasq2;
  }
});

// lib/lapack/base/dlasq1/lib/base.js
var require_base74 = __commonJS({
  "lib/lapack/base/dlasq1/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dcopy = require_base23();
    var dlamch = require_base5();
    var dlas2 = require_base66();
    var dlascl = require_base8();
    var dlasq2 = require_base73();
    var dlasrt = require_base72();
    var ZERO = 0;
    function dlasq1(N, d, strideD, offsetD, e, strideE, offsetE, WORK, strideWORK, offsetWORK) {
      var safmin;
      var sigmx;
      var sigmn;
      var scale;
      var info;
      var eps;
      var out;
      var id;
      var ie;
      var iw;
      var i;
      info = 0;
      if (N < 0) {
        return -1;
      }
      if (N === 0) {
        return 0;
      }
      if (N === 1) {
        d[offsetD] = Math.abs(d[offsetD]);
        return 0;
      }
      if (N === 2) {
        out = new Float64Array2(2);
        dlas2(d[offsetD], e[offsetE], d[offsetD + strideD], out);
        sigmn = out[0];
        sigmx = out[1];
        d[offsetD] = sigmx;
        d[offsetD + strideD] = sigmn;
        return 0;
      }
      sigmx = ZERO;
      id = offsetD;
      ie = offsetE;
      for (i = 0; i < N - 1; i++) {
        d[id] = Math.abs(d[id]);
        sigmx = Math.max(sigmx, Math.abs(e[ie]));
        id += strideD;
        ie += strideE;
      }
      d[id] = Math.abs(d[id]);
      if (sigmx === ZERO) {
        dlasrt("decreasing", N, d, strideD, offsetD);
        return 0;
      }
      id = offsetD;
      for (i = 0; i < N; i++) {
        sigmx = Math.max(sigmx, d[id]);
        id += strideD;
      }
      eps = dlamch("Precision");
      safmin = dlamch("Safe minimum");
      scale = Math.sqrt(eps / safmin);
      dcopy(N, d, strideD, offsetD, WORK, 2 * strideWORK, offsetWORK);
      dcopy(N - 1, e, strideE, offsetE, WORK, 2 * strideWORK, offsetWORK + strideWORK);
      dlascl("general", 0, 0, sigmx, scale, 2 * N - 1, 1, WORK, strideWORK, (2 * N - 1) * strideWORK, offsetWORK);
      iw = offsetWORK;
      for (i = 0; i < 2 * N - 1; i++) {
        WORK[iw] *= WORK[iw];
        iw += strideWORK;
      }
      WORK[offsetWORK + (2 * N - 1) * strideWORK] = ZERO;
      info = dlasq2(N, WORK, strideWORK, offsetWORK);
      if (info === 0) {
        iw = offsetWORK;
        id = offsetD;
        for (i = 0; i < N; i++) {
          d[id] = Math.sqrt(WORK[iw]);
          id += strideD;
          iw += strideWORK;
        }
        dlascl("general", 0, 0, scale, sigmx, N, 1, d, strideD, N * strideD, offsetD);
      } else if (info === 2) {
        id = offsetD;
        ie = offsetE;
        iw = offsetWORK;
        for (i = 0; i < N; i++) {
          d[id] = Math.sqrt(WORK[iw]);
          if (i < N - 1) {
            e[ie] = Math.sqrt(WORK[iw + strideWORK]);
            ie += strideE;
          }
          id += strideD;
          iw += 2 * strideWORK;
        }
        dlascl("general", 0, 0, scale, sigmx, N, 1, d, strideD, N * strideD, offsetD);
        dlascl("general", 0, 0, scale, sigmx, N, 1, e, strideE, N * strideE, offsetE);
      }
      return info;
    }
    module.exports = dlasq1;
  }
});

// lib/lapack/base/dlasv2/lib/base.js
var require_base75 = __commonJS({
  "lib/lapack/base/dlasv2/lib/base.js"(exports, module) {
    "use strict";
    var EPS = 11102230246251565e-32;
    function sign(a, b) {
      var mag = Math.abs(a);
      if (b > 0 || b === 0 && !Object.is(b, -0)) {
        return mag;
      }
      return -mag;
    }
    function dlasv2(f, g, h) {
      var gasmal;
      var tsign;
      var ssmin;
      var ssmax;
      var swap;
      var pmax;
      var temp;
      var csl;
      var csr;
      var snl;
      var snr;
      var clt;
      var crt;
      var slt;
      var srt;
      var ft;
      var fa;
      var gt;
      var ga;
      var ht;
      var ha;
      var mm;
      var tt;
      var d;
      var l;
      var m;
      var r;
      var s;
      var t;
      var a;
      ft = f;
      fa = Math.abs(ft);
      ht = h;
      ha = Math.abs(h);
      pmax = 1;
      swap = ha > fa;
      if (swap) {
        pmax = 3;
        temp = ft;
        ft = ht;
        ht = temp;
        temp = fa;
        fa = ha;
        ha = temp;
      }
      gt = g;
      ga = Math.abs(gt);
      if (ga === 0) {
        ssmin = ha;
        ssmax = fa;
        clt = 1;
        crt = 1;
        slt = 0;
        srt = 0;
      } else {
        gasmal = true;
        if (ga > fa) {
          pmax = 2;
          if (fa / ga < EPS) {
            gasmal = false;
            ssmax = ga;
            if (ha > 1) {
              ssmin = fa / (ga / ha);
            } else {
              ssmin = fa / ga * ha;
            }
            clt = 1;
            slt = ht / gt;
            srt = 1;
            crt = ft / gt;
          }
        }
        if (gasmal) {
          d = fa - ha;
          if (d === fa) {
            l = 1;
          } else {
            l = d / fa;
          }
          m = gt / ft;
          t = 2 - l;
          mm = m * m;
          tt = t * t;
          s = Math.sqrt(tt + mm);
          if (l === 0) {
            r = Math.abs(m);
          } else {
            r = Math.sqrt(l * l + mm);
          }
          a = 0.5 * (s + r);
          ssmin = ha / a;
          ssmax = fa * a;
          if (mm === 0) {
            if (l === 0) {
              t = sign(2, ft) * sign(1, gt);
            } else {
              t = gt / sign(d, ft) + m / t;
            }
          } else {
            t = (m / (s + t) + m / (r + l)) * (1 + a);
          }
          l = Math.sqrt(t * t + 4);
          crt = 2 / l;
          srt = t / l;
          clt = (crt + srt * m) / a;
          slt = ht / ft * srt / a;
        }
      }
      if (swap) {
        csl = srt;
        snl = crt;
        csr = slt;
        snr = clt;
      } else {
        csl = clt;
        snl = slt;
        csr = crt;
        snr = srt;
      }
      if (pmax === 1) {
        tsign = sign(1, csr) * sign(1, csl) * sign(1, f);
      }
      if (pmax === 2) {
        tsign = sign(1, snr) * sign(1, csl) * sign(1, g);
      }
      if (pmax === 3) {
        tsign = sign(1, snr) * sign(1, snl) * sign(1, h);
      }
      ssmax = sign(ssmax, tsign);
      ssmin = sign(ssmin, tsign * sign(1, f) * sign(1, h));
      return {
        "ssmin": ssmin,
        "ssmax": ssmax,
        "snr": snr,
        "csr": csr,
        "snl": snl,
        "csl": csl
      };
    }
    module.exports = dlasv2;
  }
});

// lib/lapack/base/dlasr/lib/base.js
var require_base76 = __commonJS({
  "lib/lapack/base/dlasr/lib/base.js"(exports, module) {
    "use strict";
    function dlasr(side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA) {
      var ctemp;
      var stemp;
      var temp;
      var idx1;
      var idx2;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return A;
      }
      if (side === "left") {
        if (pivot === "variable") {
          if (direct === "forward") {
            for (j = 0; j < M - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + (j + 1) * strideA1 + i * strideA2;
                  idx2 = offsetA + j * strideA1 + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          } else {
            for (j = M - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + (j + 1) * strideA1 + i * strideA2;
                  idx2 = offsetA + j * strideA1 + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          }
        } else if (pivot === "top") {
          if (direct === "forward") {
            for (j = 1; j < M; j++) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + j * strideA1 + i * strideA2;
                  idx2 = offsetA + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          } else {
            for (j = M - 1; j >= 1; j--) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + j * strideA1 + i * strideA2;
                  idx2 = offsetA + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          }
        } else if (pivot === "bottom") {
          if (direct === "forward") {
            for (j = 0; j < M - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + j * strideA1 + i * strideA2;
                  idx2 = offsetA + (M - 1) * strideA1 + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = stemp * A[idx2] + ctemp * temp;
                  A[idx2] = ctemp * A[idx2] - stemp * temp;
                }
              }
            }
          } else {
            for (j = M - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = offsetA + j * strideA1 + i * strideA2;
                  idx2 = offsetA + (M - 1) * strideA1 + i * strideA2;
                  temp = A[idx1];
                  A[idx1] = stemp * A[idx2] + ctemp * temp;
                  A[idx2] = ctemp * A[idx2] - stemp * temp;
                }
              }
            }
          }
        }
      } else if (side === "right") {
        if (pivot === "variable") {
          if (direct === "forward") {
            for (j = 0; j < N - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + (j + 1) * strideA2;
                  idx2 = offsetA + i * strideA1 + j * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          } else {
            for (j = N - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + (j + 1) * strideA2;
                  idx2 = offsetA + i * strideA1 + j * strideA2;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          }
        } else if (pivot === "top") {
          if (direct === "forward") {
            for (j = 1; j < N; j++) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + j * strideA2;
                  idx2 = offsetA + i * strideA1;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          } else {
            for (j = N - 1; j >= 1; j--) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + j * strideA2;
                  idx2 = offsetA + i * strideA1;
                  temp = A[idx1];
                  A[idx1] = ctemp * temp - stemp * A[idx2];
                  A[idx2] = stemp * temp + ctemp * A[idx2];
                }
              }
            }
          }
        } else if (pivot === "bottom") {
          if (direct === "forward") {
            for (j = 0; j < N - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + j * strideA2;
                  idx2 = offsetA + i * strideA1 + (N - 1) * strideA2;
                  temp = A[idx1];
                  A[idx1] = stemp * A[idx2] + ctemp * temp;
                  A[idx2] = ctemp * A[idx2] - stemp * temp;
                }
              }
            }
          } else {
            for (j = N - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = offsetA + i * strideA1 + j * strideA2;
                  idx2 = offsetA + i * strideA1 + (N - 1) * strideA2;
                  temp = A[idx1];
                  A[idx1] = stemp * A[idx2] + ctemp * temp;
                  A[idx2] = ctemp * A[idx2] - stemp * temp;
                }
              }
            }
          }
        }
      }
      return A;
    }
    module.exports = dlasr;
  }
});

// lib/lapack/base/dbdsqr/lib/base.js
var require_base77 = __commonJS({
  "lib/lapack/base/dbdsqr/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dlamch = require_base5();
    var dlartg = require_base40();
    var dlas2 = require_base66();
    var dlasq1 = require_base74();
    var dlasv2 = require_base75();
    var drot = require_base4();
    var dscal = require_base();
    var dlasr = require_base76();
    var dswap = require_base10();
    var ZERO = 0;
    var ONE = 1;
    var NEGONE = -1;
    var HNDRTH = 0.01;
    var TEN = 10;
    var HNDRD = 100;
    var MEIGTH = -0.125;
    var MAXITR = 6;
    var DOUT = new Float64Array2(2);
    function sign(a, b) {
      var mag = Math.abs(a);
      if (b > 0 || b === 0 && !Object.is(b, -0)) {
        return mag;
      }
      return -mag;
    }
    function dbdsqr(uplo, N, ncvt, nru, ncc, d, strideD, offsetD, e, strideE, offsetE, VT, strideVT1, strideVT2, offsetVT, U, strideU1, strideU2, offsetU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var splitFound;
      var maxitdivn;
      var converged;
      var iterdivn;
      var rotate;
      var thresh;
      var sminoa;
      var tolmul;
      var lower;
      var oldcs;
      var oldsn;
      var shift;
      var sigmn;
      var sigmx;
      var oldll;
      var oldm;
      var smax;
      var smin;
      var abse;
      var abss;
      var cosl;
      var cosr;
      var sinl;
      var sinr;
      var unfl;
      var info;
      var idir;
      var iter;
      var dout;
      var svd2;
      var nm12;
      var nm13;
      var rot;
      var eps;
      var nm1;
      var tol;
      var sll;
      var lll;
      var cs;
      var sn;
      var mu;
      var ll;
      var m;
      var f;
      var g;
      var h;
      var r;
      var i;
      dout = DOUT;
      rot = new Float64Array2(3);
      info = 0;
      if (N === 0) {
        return 0;
      }
      lower = uplo === "lower";
      if (N === 1) {
        if (d[offsetD] < ZERO) {
          d[offsetD] = -d[offsetD];
          if (ncvt > 0) {
            dscal(ncvt, NEGONE, VT, strideVT2, offsetVT);
          }
        }
        return 0;
      }
      rotate = ncvt > 0 || nru > 0 || ncc > 0;
      if (!rotate) {
        info = dlasq1(N, d, strideD, offsetD, e, strideE, offsetE, WORK, strideWORK, offsetWORK);
        if (info !== 2) {
          return info;
        }
        info = 0;
      }
      nm1 = N - 1;
      nm12 = nm1 + nm1;
      nm13 = nm12 + nm1;
      idir = 0;
      eps = dlamch("Epsilon");
      unfl = dlamch("Safe minimum");
      if (lower) {
        for (i = 0; i < N - 1; i++) {
          dlartg(d[offsetD + i * strideD], e[offsetE + i * strideE], rot);
          cs = rot[0];
          sn = rot[1];
          r = rot[2];
          d[offsetD + i * strideD] = r;
          e[offsetE + i * strideE] = sn * d[offsetD + (i + 1) * strideD];
          d[offsetD + (i + 1) * strideD] = cs * d[offsetD + (i + 1) * strideD];
          WORK[offsetWORK + i * strideWORK] = cs;
          WORK[offsetWORK + (nm1 + i) * strideWORK] = sn;
        }
        if (nru > 0) {
          dlasr(
            "right",
            "variable",
            "forward",
            nru,
            N,
            WORK,
            strideWORK,
            offsetWORK,
            WORK,
            strideWORK,
            offsetWORK + nm1 * strideWORK,
            U,
            strideU1,
            strideU2,
            offsetU
          );
        }
        if (ncc > 0) {
          dlasr(
            "left",
            "variable",
            "forward",
            N,
            ncc,
            WORK,
            strideWORK,
            offsetWORK,
            WORK,
            strideWORK,
            offsetWORK + nm1 * strideWORK,
            C,
            strideC1,
            strideC2,
            offsetC
          );
        }
      }
      tolmul = Math.max(TEN, Math.min(HNDRD, Math.pow(eps, MEIGTH)));
      tol = tolmul * eps;
      smax = ZERO;
      for (i = 0; i < N; i++) {
        smax = Math.max(smax, Math.abs(d[offsetD + i * strideD]));
      }
      for (i = 0; i < N - 1; i++) {
        smax = Math.max(smax, Math.abs(e[offsetE + i * strideE]));
      }
      smin = ZERO;
      if (tol >= ZERO) {
        sminoa = Math.abs(d[offsetD]);
        if (sminoa !== ZERO) {
          mu = sminoa;
          for (i = 1; i < N; i++) {
            mu = Math.abs(d[offsetD + i * strideD]) * (mu / (mu + Math.abs(e[offsetE + (i - 1) * strideE])));
            sminoa = Math.min(sminoa, mu);
            if (sminoa === ZERO) {
              break;
            }
          }
        }
        sminoa /= Math.sqrt(N);
        thresh = Math.max(tol * sminoa, MAXITR * (N * (N * unfl)));
      } else {
        thresh = Math.max(Math.abs(tol) * smax, MAXITR * (N * (N * unfl)));
      }
      maxitdivn = MAXITR * N;
      iterdivn = 0;
      iter = -1;
      oldll = -1;
      oldm = -1;
      m = N - 1;
      while (true) {
        if (m <= 0) {
          break;
        }
        if (iter >= N) {
          iter -= N;
          iterdivn += 1;
          if (iterdivn >= maxitdivn) {
            info = 0;
            for (i = 0; i < N - 1; i++) {
              if (e[offsetE + i * strideE] !== ZERO) {
                info += 1;
              }
            }
            sortSingularValues(N, d, strideD, offsetD, ncvt, VT, strideVT1, strideVT2, offsetVT, nru, U, strideU1, strideU2, offsetU, ncc, C, strideC1, strideC2, offsetC);
            return info;
          }
        }
        if (tol < ZERO && Math.abs(d[offsetD + m * strideD]) <= thresh) {
          d[offsetD + m * strideD] = ZERO;
        }
        smax = Math.abs(d[offsetD + m * strideD]);
        ll = -1;
        splitFound = false;
        for (lll = 0; lll < m; lll++) {
          ll = m - 1 - lll;
          abss = Math.abs(d[offsetD + ll * strideD]);
          abse = Math.abs(e[offsetE + ll * strideE]);
          if (tol < ZERO && abss <= thresh) {
            d[offsetD + ll * strideD] = ZERO;
          }
          if (abse <= thresh) {
            e[offsetE + ll * strideE] = ZERO;
            if (ll === m - 1) {
              m -= 1;
              splitFound = true;
              break;
            }
            ll += 1;
            break;
          }
          smax = Math.max(smax, abss, abse);
          if (lll === m - 1) {
            ll = 0;
            break;
          }
        }
        if (splitFound) {
          continue;
        }
        if (m === 0) {
          break;
        }
        if (ll === -1) {
          ll = 0;
        }
        if (ll === m - 1) {
          svd2 = dlasv2(
            d[offsetD + (m - 1) * strideD],
            e[offsetE + (m - 1) * strideE],
            d[offsetD + m * strideD]
          );
          sigmn = svd2.ssmin;
          sigmx = svd2.ssmax;
          sinr = svd2.snr;
          cosr = svd2.csr;
          sinl = svd2.snl;
          cosl = svd2.csl;
          d[offsetD + (m - 1) * strideD] = sigmx;
          e[offsetE + (m - 1) * strideE] = ZERO;
          d[offsetD + m * strideD] = sigmn;
          if (ncvt > 0) {
            drot(
              ncvt,
              VT,
              strideVT2,
              offsetVT + (m - 1) * strideVT1,
              VT,
              strideVT2,
              offsetVT + m * strideVT1,
              cosr,
              sinr
            );
          }
          if (nru > 0) {
            drot(
              nru,
              U,
              strideU1,
              offsetU + (m - 1) * strideU2,
              U,
              strideU1,
              offsetU + m * strideU2,
              cosl,
              sinl
            );
          }
          if (ncc > 0) {
            drot(
              ncc,
              C,
              strideC2,
              offsetC + (m - 1) * strideC1,
              C,
              strideC2,
              offsetC + m * strideC1,
              cosl,
              sinl
            );
          }
          m -= 2;
          continue;
        }
        if (ll > oldm || m < oldll) {
          if (Math.abs(d[offsetD + ll * strideD]) >= Math.abs(d[offsetD + m * strideD])) {
            idir = 1;
          } else {
            idir = 2;
          }
        }
        converged = false;
        if (idir === 1) {
          if (Math.abs(e[offsetE + (m - 1) * strideE]) <= Math.abs(tol) * Math.abs(d[offsetD + m * strideD]) || tol < ZERO && Math.abs(e[offsetE + (m - 1) * strideE]) <= thresh) {
            e[offsetE + (m - 1) * strideE] = ZERO;
            converged = true;
          }
          if (!converged && tol >= ZERO) {
            mu = Math.abs(d[offsetD + ll * strideD]);
            smin = mu;
            for (lll = ll; lll < m; lll++) {
              if (Math.abs(e[offsetE + lll * strideE]) <= tol * mu) {
                e[offsetE + lll * strideE] = ZERO;
                converged = true;
                break;
              }
              mu = Math.abs(d[offsetD + (lll + 1) * strideD]) * (mu / (mu + Math.abs(e[offsetE + lll * strideE])));
              smin = Math.min(smin, mu);
            }
          }
        } else {
          if (Math.abs(e[offsetE + ll * strideE]) <= Math.abs(tol) * Math.abs(d[offsetD + ll * strideD]) || tol < ZERO && Math.abs(e[offsetE + ll * strideE]) <= thresh) {
            e[offsetE + ll * strideE] = ZERO;
            converged = true;
          }
          if (!converged && tol >= ZERO) {
            mu = Math.abs(d[offsetD + m * strideD]);
            smin = mu;
            for (lll = m - 1; lll >= ll; lll--) {
              if (Math.abs(e[offsetE + lll * strideE]) <= tol * mu) {
                e[offsetE + lll * strideE] = ZERO;
                converged = true;
                break;
              }
              mu = Math.abs(d[offsetD + lll * strideD]) * (mu / (mu + Math.abs(e[offsetE + lll * strideE])));
              smin = Math.min(smin, mu);
            }
          }
        }
        if (converged) {
          continue;
        }
        oldll = ll;
        oldm = m;
        if (tol >= ZERO && N * tol * (smin / smax) <= Math.max(eps, HNDRTH * tol)) {
          shift = ZERO;
        } else {
          if (idir === 1) {
            sll = Math.abs(d[offsetD + ll * strideD]);
            dlas2(
              d[offsetD + (m - 1) * strideD],
              e[offsetE + (m - 1) * strideE],
              d[offsetD + m * strideD],
              dout
            );
            shift = dout[0];
            r = dout[1];
          } else {
            sll = Math.abs(d[offsetD + m * strideD]);
            dlas2(
              d[offsetD + ll * strideD],
              e[offsetE + ll * strideE],
              d[offsetD + (ll + 1) * strideD],
              dout
            );
            shift = dout[0];
            r = dout[1];
          }
          if (sll > ZERO) {
            if (shift / sll * (shift / sll) < eps) {
              shift = ZERO;
            }
          }
        }
        iter = iter + m - ll;
        if (shift === ZERO) {
          if (idir === 1) {
            cs = ONE;
            oldcs = ONE;
            for (i = ll; i < m; i++) {
              dlartg(d[offsetD + i * strideD] * cs, e[offsetE + i * strideE], rot);
              cs = rot[0];
              sn = rot[1];
              r = rot[2];
              if (i > ll) {
                e[offsetE + (i - 1) * strideE] = oldsn * r;
              }
              dlartg(oldcs * r, d[offsetD + (i + 1) * strideD] * sn, rot);
              oldcs = rot[0];
              oldsn = rot[1];
              d[offsetD + i * strideD] = rot[2];
              WORK[offsetWORK + (i - ll) * strideWORK] = cs;
              WORK[offsetWORK + (i - ll + nm1) * strideWORK] = sn;
              WORK[offsetWORK + (i - ll + nm12) * strideWORK] = oldcs;
              WORK[offsetWORK + (i - ll + nm13) * strideWORK] = oldsn;
            }
            h = d[offsetD + m * strideD] * cs;
            d[offsetD + m * strideD] = h * oldcs;
            e[offsetE + (m - 1) * strideE] = h * oldsn;
            if (ncvt > 0) {
              dlasr(
                "left",
                "variable",
                "forward",
                m - ll + 1,
                ncvt,
                WORK,
                strideWORK,
                offsetWORK,
                WORK,
                strideWORK,
                offsetWORK + nm1 * strideWORK,
                VT,
                strideVT1,
                strideVT2,
                offsetVT + ll * strideVT1
              );
            }
            if (nru > 0) {
              dlasr(
                "right",
                "variable",
                "forward",
                nru,
                m - ll + 1,
                WORK,
                strideWORK,
                offsetWORK + nm12 * strideWORK,
                WORK,
                strideWORK,
                offsetWORK + nm13 * strideWORK,
                U,
                strideU1,
                strideU2,
                offsetU + ll * strideU2
              );
            }
            if (ncc > 0) {
              dlasr(
                "left",
                "variable",
                "forward",
                m - ll + 1,
                ncc,
                WORK,
                strideWORK,
                offsetWORK + nm12 * strideWORK,
                WORK,
                strideWORK,
                offsetWORK + nm13 * strideWORK,
                C,
                strideC1,
                strideC2,
                offsetC + ll * strideC1
              );
            }
            if (Math.abs(e[offsetE + (m - 1) * strideE]) <= thresh) {
              e[offsetE + (m - 1) * strideE] = ZERO;
            }
          } else {
            cs = ONE;
            oldcs = ONE;
            for (i = m; i >= ll + 1; i--) {
              dlartg(d[offsetD + i * strideD] * cs, e[offsetE + (i - 1) * strideE], rot);
              cs = rot[0];
              sn = rot[1];
              r = rot[2];
              if (i < m) {
                e[offsetE + i * strideE] = oldsn * r;
              }
              dlartg(oldcs * r, d[offsetD + (i - 1) * strideD] * sn, rot);
              oldcs = rot[0];
              oldsn = rot[1];
              d[offsetD + i * strideD] = rot[2];
              WORK[offsetWORK + (i - ll - 1) * strideWORK] = cs;
              WORK[offsetWORK + (i - ll - 1 + nm1) * strideWORK] = -sn;
              WORK[offsetWORK + (i - ll - 1 + nm12) * strideWORK] = oldcs;
              WORK[offsetWORK + (i - ll - 1 + nm13) * strideWORK] = -oldsn;
            }
            h = d[offsetD + ll * strideD] * cs;
            d[offsetD + ll * strideD] = h * oldcs;
            e[offsetE + ll * strideE] = h * oldsn;
            if (ncvt > 0) {
              dlasr(
                "left",
                "variable",
                "backward",
                m - ll + 1,
                ncvt,
                WORK,
                strideWORK,
                offsetWORK + nm12 * strideWORK,
                WORK,
                strideWORK,
                offsetWORK + nm13 * strideWORK,
                VT,
                strideVT1,
                strideVT2,
                offsetVT + ll * strideVT1
              );
            }
            if (nru > 0) {
              dlasr(
                "right",
                "variable",
                "backward",
                nru,
                m - ll + 1,
                WORK,
                strideWORK,
                offsetWORK,
                WORK,
                strideWORK,
                offsetWORK + nm1 * strideWORK,
                U,
                strideU1,
                strideU2,
                offsetU + ll * strideU2
              );
            }
            if (ncc > 0) {
              dlasr(
                "left",
                "variable",
                "backward",
                m - ll + 1,
                ncc,
                WORK,
                strideWORK,
                offsetWORK,
                WORK,
                strideWORK,
                offsetWORK + nm1 * strideWORK,
                C,
                strideC1,
                strideC2,
                offsetC + ll * strideC1
              );
            }
            if (Math.abs(e[offsetE + ll * strideE]) <= thresh) {
              e[offsetE + ll * strideE] = ZERO;
            }
          }
        } else if (idir === 1) {
          f = (Math.abs(d[offsetD + ll * strideD]) - shift) * (sign(ONE, d[offsetD + ll * strideD]) + shift / d[offsetD + ll * strideD]);
          g = e[offsetE + ll * strideE];
          for (i = ll; i < m; i++) {
            dlartg(f, g, rot);
            cosr = rot[0];
            sinr = rot[1];
            r = rot[2];
            if (i > ll) {
              e[offsetE + (i - 1) * strideE] = r;
            }
            f = cosr * d[offsetD + i * strideD] + sinr * e[offsetE + i * strideE];
            e[offsetE + i * strideE] = cosr * e[offsetE + i * strideE] - sinr * d[offsetD + i * strideD];
            g = sinr * d[offsetD + (i + 1) * strideD];
            d[offsetD + (i + 1) * strideD] = cosr * d[offsetD + (i + 1) * strideD];
            dlartg(f, g, rot);
            cosl = rot[0];
            sinl = rot[1];
            d[offsetD + i * strideD] = rot[2];
            f = cosl * e[offsetE + i * strideE] + sinl * d[offsetD + (i + 1) * strideD];
            d[offsetD + (i + 1) * strideD] = cosl * d[offsetD + (i + 1) * strideD] - sinl * e[offsetE + i * strideE];
            if (i < m - 1) {
              g = sinl * e[offsetE + (i + 1) * strideE];
              e[offsetE + (i + 1) * strideE] = cosl * e[offsetE + (i + 1) * strideE];
            }
            WORK[offsetWORK + (i - ll) * strideWORK] = cosr;
            WORK[offsetWORK + (i - ll + nm1) * strideWORK] = sinr;
            WORK[offsetWORK + (i - ll + nm12) * strideWORK] = cosl;
            WORK[offsetWORK + (i - ll + nm13) * strideWORK] = sinl;
          }
          e[offsetE + (m - 1) * strideE] = f;
          if (ncvt > 0) {
            dlasr(
              "left",
              "variable",
              "forward",
              m - ll + 1,
              ncvt,
              WORK,
              strideWORK,
              offsetWORK,
              WORK,
              strideWORK,
              offsetWORK + nm1 * strideWORK,
              VT,
              strideVT1,
              strideVT2,
              offsetVT + ll * strideVT1
            );
          }
          if (nru > 0) {
            dlasr(
              "right",
              "variable",
              "forward",
              nru,
              m - ll + 1,
              WORK,
              strideWORK,
              offsetWORK + nm12 * strideWORK,
              WORK,
              strideWORK,
              offsetWORK + nm13 * strideWORK,
              U,
              strideU1,
              strideU2,
              offsetU + ll * strideU2
            );
          }
          if (ncc > 0) {
            dlasr(
              "left",
              "variable",
              "forward",
              m - ll + 1,
              ncc,
              WORK,
              strideWORK,
              offsetWORK + nm12 * strideWORK,
              WORK,
              strideWORK,
              offsetWORK + nm13 * strideWORK,
              C,
              strideC1,
              strideC2,
              offsetC + ll * strideC1
            );
          }
          if (Math.abs(e[offsetE + (m - 1) * strideE]) <= thresh) {
            e[offsetE + (m - 1) * strideE] = ZERO;
          }
        } else {
          f = (Math.abs(d[offsetD + m * strideD]) - shift) * (sign(ONE, d[offsetD + m * strideD]) + shift / d[offsetD + m * strideD]);
          g = e[offsetE + (m - 1) * strideE];
          for (i = m; i >= ll + 1; i--) {
            dlartg(f, g, rot);
            cosr = rot[0];
            sinr = rot[1];
            r = rot[2];
            if (i < m) {
              e[offsetE + i * strideE] = r;
            }
            f = cosr * d[offsetD + i * strideD] + sinr * e[offsetE + (i - 1) * strideE];
            e[offsetE + (i - 1) * strideE] = cosr * e[offsetE + (i - 1) * strideE] - sinr * d[offsetD + i * strideD];
            g = sinr * d[offsetD + (i - 1) * strideD];
            d[offsetD + (i - 1) * strideD] = cosr * d[offsetD + (i - 1) * strideD];
            dlartg(f, g, rot);
            cosl = rot[0];
            sinl = rot[1];
            d[offsetD + i * strideD] = rot[2];
            f = cosl * e[offsetE + (i - 1) * strideE] + sinl * d[offsetD + (i - 1) * strideD];
            d[offsetD + (i - 1) * strideD] = cosl * d[offsetD + (i - 1) * strideD] - sinl * e[offsetE + (i - 1) * strideE];
            if (i > ll + 1) {
              g = sinl * e[offsetE + (i - 2) * strideE];
              e[offsetE + (i - 2) * strideE] = cosl * e[offsetE + (i - 2) * strideE];
            }
            WORK[offsetWORK + (i - ll - 1) * strideWORK] = cosr;
            WORK[offsetWORK + (i - ll - 1 + nm1) * strideWORK] = -sinr;
            WORK[offsetWORK + (i - ll - 1 + nm12) * strideWORK] = cosl;
            WORK[offsetWORK + (i - ll - 1 + nm13) * strideWORK] = -sinl;
          }
          e[offsetE + ll * strideE] = f;
          if (Math.abs(e[offsetE + ll * strideE]) <= thresh) {
            e[offsetE + ll * strideE] = ZERO;
          }
          if (ncvt > 0) {
            dlasr(
              "left",
              "variable",
              "backward",
              m - ll + 1,
              ncvt,
              WORK,
              strideWORK,
              offsetWORK + nm12 * strideWORK,
              WORK,
              strideWORK,
              offsetWORK + nm13 * strideWORK,
              VT,
              strideVT1,
              strideVT2,
              offsetVT + ll * strideVT1
            );
          }
          if (nru > 0) {
            dlasr(
              "right",
              "variable",
              "backward",
              nru,
              m - ll + 1,
              WORK,
              strideWORK,
              offsetWORK,
              WORK,
              strideWORK,
              offsetWORK + nm1 * strideWORK,
              U,
              strideU1,
              strideU2,
              offsetU + ll * strideU2
            );
          }
          if (ncc > 0) {
            dlasr(
              "left",
              "variable",
              "backward",
              m - ll + 1,
              ncc,
              WORK,
              strideWORK,
              offsetWORK,
              WORK,
              strideWORK,
              offsetWORK + nm1 * strideWORK,
              C,
              strideC1,
              strideC2,
              offsetC + ll * strideC1
            );
          }
        }
      }
      sortSingularValues(N, d, strideD, offsetD, ncvt, VT, strideVT1, strideVT2, offsetVT, nru, U, strideU1, strideU2, offsetU, ncc, C, strideC1, strideC2, offsetC);
      return info;
    }
    function sortSingularValues(N, d, strideD, offsetD, ncvt, VT, strideVT1, strideVT2, offsetVT, nru, U, strideU1, strideU2, offsetU, ncc, C, strideC1, strideC2, offsetC) {
      var smin;
      var isub;
      var i;
      var j;
      for (i = 0; i < N; i++) {
        if (d[offsetD + i * strideD] < ZERO) {
          d[offsetD + i * strideD] = -d[offsetD + i * strideD];
          if (ncvt > 0) {
            dscal(ncvt, NEGONE, VT, strideVT2, offsetVT + i * strideVT1);
          }
        }
      }
      for (i = 0; i < N - 1; i++) {
        isub = 0;
        smin = d[offsetD];
        for (j = 1; j < N - i; j++) {
          if (d[offsetD + j * strideD] <= smin) {
            isub = j;
            smin = d[offsetD + j * strideD];
          }
        }
        if (isub !== N - 1 - i) {
          d[offsetD + isub * strideD] = d[offsetD + (N - 1 - i) * strideD];
          d[offsetD + (N - 1 - i) * strideD] = smin;
          if (ncvt > 0) {
            dswap(
              ncvt,
              VT,
              strideVT2,
              offsetVT + isub * strideVT1,
              VT,
              strideVT2,
              offsetVT + (N - 1 - i) * strideVT1
            );
          }
          if (nru > 0) {
            dswap(
              nru,
              U,
              strideU1,
              offsetU + isub * strideU2,
              U,
              strideU1,
              offsetU + (N - 1 - i) * strideU2
            );
          }
          if (ncc > 0) {
            dswap(
              ncc,
              C,
              strideC2,
              offsetC + isub * strideC1,
              C,
              strideC2,
              offsetC + (N - 1 - i) * strideC1
            );
          }
        }
      }
    }
    module.exports = dbdsqr;
  }
});

// lib/lapack/base/dgesvd/lib/base.js
var require_base78 = __commonJS({
  "lib/lapack/base/dgesvd/lib/base.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib8();
    var dlamch = require_base5();
    var dlange = require_base7();
    var dlascl = require_base8();
    var dlacpy = require_base9();
    var dlaset = require_base35();
    var dgeqrf = require_base57();
    var dorgqr = require_base31();
    var dgelqf = require_base59();
    var dorglq = require_base61();
    var dgebrd = require_base64();
    var dorgbr = require_base65();
    var dbdsqr = require_base77();
    var dgemm2 = require_base22();
    var MNTHR_FAC = 1.6;
    function dgesvd2(jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT) {
      var wntuas;
      var wntvas;
      var bignum;
      var smlnum;
      var ldwrkr;
      var ldwrku;
      var wntua;
      var wntus;
      var wntuo;
      var wntun;
      var wntva;
      var wntvs;
      var wntvo;
      var wntvn;
      var minmn;
      var mnthr;
      var itauq;
      var itaup;
      var iwork;
      var chunk;
      var anrm;
      var iscl;
      var info;
      var ncvt;
      var nrvt;
      var itau;
      var svt1;
      var svt2;
      var eps;
      var ncu;
      var nru;
      var blk;
      var sa1;
      var sa2;
      var su1;
      var su2;
      var wsz;
      var DUM;
      var ie;
      var ir;
      var iu;
      var WK;
      var i;
      sa1 = strideA1;
      sa2 = strideA2;
      su1 = strideU1;
      su2 = strideU2;
      svt1 = strideVT1;
      svt2 = strideVT2;
      info = 0;
      minmn = Math.min(M, N);
      wntua = jobu === "all-columns";
      wntus = jobu === "economy";
      wntuas = wntua || wntus;
      wntuo = jobu === "overwrite";
      wntun = jobu === "none";
      wntva = jobvt === "all-rows";
      wntvs = jobvt === "economy";
      wntvas = wntva || wntvs;
      wntvo = jobvt === "overwrite";
      wntvn = jobvt === "none";
      if (M === 0 || N === 0) {
        return 0;
      }
      wsz = Math.max(
        1,
        5 * minmn,
        // bdspac
        3 * minmn + Math.max(M, N),
        // general
        2 * Math.max(M, N) * Math.max(M, N) + // two temp NxN or MxM matrices
        3 * Math.max(M, N) + Math.max(M, N)
        // + tau + work
      );
      WK = new Float64Array2(wsz);
      DUM = new Float64Array2(1);
      eps = dlamch("precision");
      smlnum = Math.sqrt(dlamch("safe-minimum")) / eps;
      bignum = 1 / smlnum;
      anrm = dlange("max", M, N, A, sa1, sa2, offsetA, DUM, 1, 0);
      iscl = 0;
      if (anrm > 0 && anrm < smlnum) {
        iscl = 1;
        dlascl("general", 0, 0, anrm, smlnum, M, N, A, sa1, sa2, offsetA);
      } else if (anrm > bignum) {
        iscl = 1;
        dlascl("general", 0, 0, anrm, bignum, M, N, A, sa1, sa2, offsetA);
      }
      mnthr = Math.floor(MNTHR_FAC * Math.min(M, N));
      if (M >= N) {
        if (M >= mnthr) {
          if (wntun) {
            itau = 0;
            iwork = itau + N;
            dgeqrf(
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itau,
              WK,
              1,
              iwork
            );
            if (N > 1) {
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                A,
                sa1,
                sa2,
                offsetA + sa1
              );
            }
            ie = 0;
            itauq = ie + N;
            itaup = itauq + N;
            iwork = itaup + N;
            dgebrd(
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            ncvt = 0;
            if (wntvo || wntvas) {
              dorgbr("apply-P", N, N, N, A, sa1, sa2, offsetA, WK, 1, itaup, WK, 1, iwork);
              ncvt = N;
            }
            iwork = ie + N;
            info = dbdsqr(
              "upper",
              N,
              ncvt,
              0,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              A,
              sa1,
              sa2,
              offsetA,
              // VT (or dummy if ncvt=0)
              DUM,
              1,
              1,
              0,
              // U dummy (nru=0)
              DUM,
              1,
              1,
              0,
              // C dummy (ncc=0)
              WK,
              1,
              iwork
            );
            if (wntvas) {
              dlacpy(
                "full",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                VT,
                svt1,
                svt2,
                offsetVT
              );
            }
          } else if (wntuo && wntvn) {
            ir = 0;
            ldwrkr = N;
            itau = ir + ldwrkr * N;
            iwork = itau + N;
            dgeqrf(
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itau,
              WK,
              1,
              iwork
            );
            dlacpy(
              "upper",
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrkr,
              ir
            );
            dlaset(
              "lower",
              N - 1,
              N - 1,
              0,
              0,
              WK,
              1,
              ldwrkr,
              ir + 1
            );
            dorgqr(M, N, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + N;
            itaup = itauq + N;
            iwork = itaup + N;
            dgebrd(
              N,
              N,
              WK,
              1,
              ldwrkr,
              ir,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dorgbr("apply-Q", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
            iwork = ie + N;
            info = dbdsqr(
              "upper",
              N,
              0,
              N,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              DUM,
              1,
              1,
              0,
              // VT dummy
              WK,
              1,
              ldwrkr,
              ir,
              // U = WORK(IR)
              DUM,
              1,
              1,
              0,
              // C dummy
              WK,
              1,
              iwork
            );
            iu = ie + N;
            ldwrku = N;
            for (i = 0; i < M; i += ldwrku) {
              chunk = Math.min(M - i, ldwrku);
              dgemm2(
                "no-transpose",
                "no-transpose",
                chunk,
                N,
                N,
                1,
                A,
                sa1,
                sa2,
                offsetA + i * sa1,
                WK,
                1,
                ldwrkr,
                ir,
                0,
                WK,
                1,
                ldwrku,
                iu
              );
              dlacpy(
                "full",
                chunk,
                N,
                WK,
                1,
                ldwrku,
                iu,
                A,
                sa1,
                sa2,
                offsetA + i * sa1
              );
            }
          } else if (wntuo && wntvas) {
            ir = 0;
            ldwrkr = N;
            itau = ir + ldwrkr * N;
            iwork = itau + N;
            dgeqrf(
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itau,
              WK,
              1,
              iwork
            );
            dlacpy(
              "upper",
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            if (N > 1) {
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                VT,
                svt1,
                svt2,
                offsetVT + svt1
              );
            }
            dorgqr(M, N, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + N;
            itaup = itauq + N;
            iwork = itaup + N;
            dgebrd(
              N,
              N,
              VT,
              svt1,
              svt2,
              offsetVT,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dlacpy(
              "lower",
              N,
              N,
              VT,
              svt1,
              svt2,
              offsetVT,
              WK,
              1,
              ldwrkr,
              ir
            );
            dorgbr("apply-Q", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
            dorgbr("apply-P", N, N, N, VT, svt1, svt2, offsetVT, WK, 1, itaup, WK, 1, iwork);
            iwork = ie + N;
            info = dbdsqr(
              "upper",
              N,
              N,
              N,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              VT,
              svt1,
              svt2,
              offsetVT,
              WK,
              1,
              ldwrkr,
              ir,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            iu = ie + N;
            ldwrku = N;
            for (i = 0; i < M; i += ldwrku) {
              chunk = Math.min(M - i, ldwrku);
              dgemm2(
                "no-transpose",
                "no-transpose",
                chunk,
                N,
                N,
                1,
                A,
                sa1,
                sa2,
                offsetA + i * sa1,
                WK,
                1,
                ldwrkr,
                ir,
                0,
                WK,
                1,
                ldwrku,
                iu
              );
              dlacpy(
                "full",
                chunk,
                N,
                WK,
                1,
                ldwrku,
                iu,
                A,
                sa1,
                sa2,
                offsetA + i * sa1
              );
            }
          } else if (wntus) {
            if (wntvn) {
              ir = 0;
              ldwrkr = N;
              itau = ir + ldwrkr * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrkr,
                ir
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrkr,
                ir + 1
              );
              dorgqr(M, N, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrkr,
                ir,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                0,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                ldwrkr,
                ir,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrkr,
                ir,
                0,
                U,
                su1,
                su2,
                offsetU
              );
            } else if (wntvo) {
              iu = 0;
              ldwrku = N;
              ir = iu + ldwrku * N;
              ldwrkr = N;
              itau = ir + ldwrkr * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrku,
                iu + 1
              );
              dorgqr(M, N, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                WK,
                1,
                ldwrkr,
                ir
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrku, iu, WK, 1, itauq, WK, 1, iwork);
              dorgbr("apply-P", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                N,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                ldwrkr,
                ir,
                // VT = WORK(IR)
                WK,
                1,
                ldwrku,
                iu,
                // U = WORK(IU)
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu,
                0,
                U,
                su1,
                su2,
                offsetU
              );
              dlacpy(
                "full",
                N,
                N,
                WK,
                1,
                ldwrkr,
                ir,
                A,
                sa1,
                sa2,
                offsetA
              );
            } else if (wntvas) {
              iu = 0;
              ldwrku = N;
              itau = iu + ldwrku * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrku,
                iu + 1
              );
              dorgqr(M, N, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                VT,
                svt1,
                svt2,
                offsetVT
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrku, iu, WK, 1, itauq, WK, 1, iwork);
              dorgbr("apply-P", N, N, N, VT, svt1, svt2, offsetVT, WK, 1, itaup, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                N,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                VT,
                svt1,
                svt2,
                offsetVT,
                WK,
                1,
                ldwrku,
                iu,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu,
                0,
                U,
                su1,
                su2,
                offsetU
              );
            }
          } else if (wntua) {
            if (wntvn) {
              ir = 0;
              ldwrkr = N;
              itau = ir + ldwrkr * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "lower",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrkr,
                ir
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrkr,
                ir + 1
              );
              dorgqr(M, M, N, U, su1, su2, offsetU, WK, 1, itau, WK, 1, iwork);
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrkr,
                ir,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                0,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                ldwrkr,
                ir,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                U,
                su1,
                su2,
                offsetU,
                WK,
                1,
                ldwrkr,
                ir,
                0,
                A,
                sa1,
                sa2,
                offsetA
              );
              dlacpy(
                "full",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
            } else if (wntvo) {
              iu = 0;
              ldwrku = N;
              ir = iu + ldwrku * N;
              ldwrkr = N;
              itau = ir + ldwrkr * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "lower",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
              dorgqr(M, M, N, U, su1, su2, offsetU, WK, 1, itau, WK, 1, iwork);
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrku,
                iu + 1
              );
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                WK,
                1,
                ldwrkr,
                ir
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrku, iu, WK, 1, itauq, WK, 1, iwork);
              dorgbr("apply-P", N, N, N, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                N,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                ldwrkr,
                ir,
                WK,
                1,
                ldwrku,
                iu,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                U,
                su1,
                su2,
                offsetU,
                WK,
                1,
                ldwrku,
                iu,
                0,
                A,
                sa1,
                sa2,
                offsetA
              );
              dlacpy(
                "full",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
              dlacpy(
                "full",
                N,
                N,
                WK,
                1,
                ldwrkr,
                ir,
                A,
                sa1,
                sa2,
                offsetA
              );
            } else if (wntvas) {
              iu = 0;
              ldwrku = N;
              itau = iu + ldwrku * N;
              iwork = itau + N;
              dgeqrf(
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                itau,
                WK,
                1,
                iwork
              );
              dlacpy(
                "lower",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
              dorgqr(M, M, N, U, su1, su2, offsetU, WK, 1, itau, WK, 1, iwork);
              dlacpy(
                "upper",
                N,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                WK,
                1,
                ldwrku,
                iu
              );
              dlaset(
                "lower",
                N - 1,
                N - 1,
                0,
                0,
                WK,
                1,
                ldwrku,
                iu + 1
              );
              ie = itau;
              itauq = ie + N;
              itaup = itauq + N;
              iwork = itaup + N;
              dgebrd(
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                WK,
                1,
                itauq,
                WK,
                1,
                itaup,
                WK,
                1,
                iwork,
                wsz - iwork
              );
              dlacpy(
                "upper",
                N,
                N,
                WK,
                1,
                ldwrku,
                iu,
                VT,
                svt1,
                svt2,
                offsetVT
              );
              dorgbr("apply-Q", N, N, N, WK, 1, ldwrku, iu, WK, 1, itauq, WK, 1, iwork);
              dorgbr("apply-P", N, N, N, VT, svt1, svt2, offsetVT, WK, 1, itaup, WK, 1, iwork);
              iwork = ie + N;
              info = dbdsqr(
                "upper",
                N,
                N,
                N,
                0,
                s,
                strideS,
                offsetS,
                WK,
                1,
                ie,
                VT,
                svt1,
                svt2,
                offsetVT,
                WK,
                1,
                ldwrku,
                iu,
                DUM,
                1,
                1,
                0,
                WK,
                1,
                iwork
              );
              dgemm2(
                "no-transpose",
                "no-transpose",
                M,
                N,
                N,
                1,
                U,
                su1,
                su2,
                offsetU,
                WK,
                1,
                ldwrku,
                iu,
                0,
                A,
                sa1,
                sa2,
                offsetA
              );
              dlacpy(
                "full",
                M,
                N,
                A,
                sa1,
                sa2,
                offsetA,
                U,
                su1,
                su2,
                offsetU
              );
            }
          }
        } else {
          ie = 0;
          itauq = ie + N;
          itaup = itauq + N;
          iwork = itaup + N;
          dgebrd(
            M,
            N,
            A,
            sa1,
            sa2,
            offsetA,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            itauq,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            wsz - iwork
          );
          if (wntuas) {
            dlacpy(
              "lower",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              U,
              su1,
              su2,
              offsetU
            );
            ncu = wntus ? N : M;
            dorgbr("apply-Q", M, ncu, N, U, su1, su2, offsetU, WK, 1, itauq, WK, 1, iwork);
          }
          if (wntvas) {
            dlacpy(
              "upper",
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dorgbr("apply-P", N, N, N, VT, svt1, svt2, offsetVT, WK, 1, itaup, WK, 1, iwork);
          }
          if (wntuo) {
            dorgbr("apply-Q", M, N, N, A, sa1, sa2, offsetA, WK, 1, itauq, WK, 1, iwork);
          }
          if (wntvo) {
            dorgbr("apply-P", N, N, N, A, sa1, sa2, offsetA, WK, 1, itaup, WK, 1, iwork);
          }
          iwork = ie + N;
          nru = 0;
          ncvt = 0;
          if (wntuas || wntuo) {
            nru = M;
          }
          if (wntvas || wntvo) {
            ncvt = N;
          }
          if (!wntuo && !wntvo) {
            info = dbdsqr(
              "upper",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              VT,
              svt1,
              svt2,
              offsetVT,
              U,
              su1,
              su2,
              offsetU,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
          } else if (!wntuo && wntvo) {
            info = dbdsqr(
              "upper",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              A,
              sa1,
              sa2,
              offsetA,
              U,
              su1,
              su2,
              offsetU,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
          } else {
            info = dbdsqr(
              "upper",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              VT,
              svt1,
              svt2,
              offsetVT,
              A,
              sa1,
              sa2,
              offsetA,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
          }
        }
      } else if (N >= mnthr) {
        if (wntvn) {
          itau = 0;
          iwork = itau + M;
          dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
          if (M > 1) {
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              A,
              sa1,
              sa2,
              offsetA + sa2
            );
          }
          ie = 0;
          itauq = ie + M;
          itaup = itauq + M;
          iwork = itaup + M;
          dgebrd(
            M,
            M,
            A,
            sa1,
            sa2,
            offsetA,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            itauq,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            wsz - iwork
          );
          if (wntuo || wntuas) {
            dorgbr("apply-Q", M, M, M, A, sa1, sa2, offsetA, WK, 1, itauq, WK, 1, iwork);
          }
          iwork = ie + M;
          nru = 0;
          if (wntuo || wntuas) {
            nru = M;
          }
          info = dbdsqr(
            "upper",
            M,
            0,
            nru,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            DUM,
            1,
            1,
            0,
            A,
            sa1,
            sa2,
            offsetA,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
          if (wntuas) {
            dlacpy(
              "full",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              U,
              su1,
              su2,
              offsetU
            );
          }
        } else if (wntvo && wntun) {
          ir = 0;
          ldwrkr = M;
          itau = ir + ldwrkr * M;
          iwork = itau + M;
          dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
          dlacpy(
            "lower",
            M,
            M,
            A,
            sa1,
            sa2,
            offsetA,
            WK,
            1,
            ldwrkr,
            ir
          );
          dlaset(
            "upper",
            M - 1,
            M - 1,
            0,
            0,
            WK,
            1,
            ldwrkr,
            ir + ldwrkr
          );
          dorglq(M, N, M, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
          ie = itau;
          itauq = ie + M;
          itaup = itauq + M;
          iwork = itaup + M;
          dgebrd(
            M,
            M,
            WK,
            1,
            ldwrkr,
            ir,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            itauq,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            wsz - iwork
          );
          dorgbr("apply-P", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
          iwork = ie + M;
          info = dbdsqr(
            "upper",
            M,
            M,
            0,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            ldwrkr,
            ir,
            DUM,
            1,
            1,
            0,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
          iu = ie + M;
          ldwrku = M;
          chunk = Math.max(1, Math.floor((wsz - iu) / M));
          for (i = 0; i < N; i += chunk) {
            blk = Math.min(N - i, chunk);
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              blk,
              M,
              1,
              WK,
              1,
              ldwrkr,
              ir,
              A,
              sa1,
              sa2,
              offsetA + i * sa2,
              0,
              WK,
              1,
              ldwrku,
              iu
            );
            dlacpy(
              "full",
              M,
              blk,
              WK,
              1,
              ldwrku,
              iu,
              A,
              sa1,
              sa2,
              offsetA + i * sa2
            );
          }
        } else if (wntvo && wntuas) {
          ir = 0;
          ldwrkr = M;
          itau = ir + ldwrkr * M;
          iwork = itau + M;
          dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
          dlacpy(
            "lower",
            M,
            M,
            A,
            sa1,
            sa2,
            offsetA,
            U,
            su1,
            su2,
            offsetU
          );
          dlaset(
            "upper",
            M - 1,
            M - 1,
            0,
            0,
            U,
            su1,
            su2,
            offsetU + su2
          );
          dorglq(M, N, M, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
          ie = itau;
          itauq = ie + M;
          itaup = itauq + M;
          iwork = itaup + M;
          dgebrd(
            M,
            M,
            U,
            su1,
            su2,
            offsetU,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            itauq,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            wsz - iwork
          );
          dlacpy(
            "upper",
            M,
            M,
            U,
            su1,
            su2,
            offsetU,
            WK,
            1,
            ldwrkr,
            ir
          );
          dorgbr("apply-P", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
          dorgbr("apply-Q", M, M, M, U, su1, su2, offsetU, WK, 1, itauq, WK, 1, iwork);
          iwork = ie + M;
          info = dbdsqr(
            "upper",
            M,
            M,
            M,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            WK,
            1,
            ldwrkr,
            ir,
            U,
            su1,
            su2,
            offsetU,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
          iu = ie + M;
          ldwrku = M;
          chunk = Math.max(1, Math.floor((wsz - iu) / M));
          for (i = 0; i < N; i += chunk) {
            blk = Math.min(N - i, chunk);
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              blk,
              M,
              1,
              WK,
              1,
              ldwrkr,
              ir,
              A,
              sa1,
              sa2,
              offsetA + i * sa2,
              0,
              WK,
              1,
              ldwrku,
              iu
            );
            dlacpy(
              "full",
              M,
              blk,
              WK,
              1,
              ldwrku,
              iu,
              A,
              sa1,
              sa2,
              offsetA + i * sa2
            );
          }
        } else if (wntvs) {
          if (wntun) {
            ir = 0;
            ldwrkr = M;
            itau = ir + ldwrkr * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrkr,
              ir
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrkr,
              ir + ldwrkr
            );
            dorglq(M, N, M, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrkr,
              ir,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              0,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrkr,
              ir,
              DUM,
              1,
              1,
              0,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrkr,
              ir,
              A,
              sa1,
              sa2,
              offsetA,
              0,
              VT,
              svt1,
              svt2,
              offsetVT
            );
          } else if (wntuo) {
            iu = 0;
            ldwrku = M;
            ir = iu + ldwrku * M;
            ldwrkr = M;
            itau = ir + ldwrkr * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrku,
              iu
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrku,
              iu + ldwrku
            );
            dorglq(M, N, M, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dlacpy(
              "lower",
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              WK,
              1,
              ldwrkr,
              ir
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrku, iu, WK, 1, itaup, WK, 1, iwork);
            dorgbr("apply-Q", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              M,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrku,
              iu,
              WK,
              1,
              ldwrkr,
              ir,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrku,
              iu,
              A,
              sa1,
              sa2,
              offsetA,
              0,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dlacpy(
              "full",
              M,
              M,
              WK,
              1,
              ldwrkr,
              ir,
              A,
              sa1,
              sa2,
              offsetA
            );
          } else if (wntuas) {
            iu = 0;
            ldwrku = M;
            itau = iu + ldwrku * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrku,
              iu
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrku,
              iu + ldwrku
            );
            dorglq(M, N, M, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dlacpy(
              "lower",
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              U,
              su1,
              su2,
              offsetU
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrku, iu, WK, 1, itaup, WK, 1, iwork);
            dorgbr("apply-Q", M, M, M, U, su1, su2, offsetU, WK, 1, itauq, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              M,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrku,
              iu,
              U,
              su1,
              su2,
              offsetU,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrku,
              iu,
              A,
              sa1,
              sa2,
              offsetA,
              0,
              VT,
              svt1,
              svt2,
              offsetVT
            );
          }
        } else if (wntva) {
          if (wntun) {
            ir = 0;
            ldwrkr = M;
            itau = ir + ldwrkr * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "upper",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrkr,
              ir
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrkr,
              ir + ldwrkr
            );
            dorglq(N, N, M, VT, svt1, svt2, offsetVT, WK, 1, itau, WK, 1, iwork);
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrkr,
              ir,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itaup, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              0,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrkr,
              ir,
              DUM,
              1,
              1,
              0,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrkr,
              ir,
              VT,
              svt1,
              svt2,
              offsetVT,
              0,
              A,
              sa1,
              sa2,
              offsetA
            );
            dlacpy(
              "full",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
          } else if (wntuo) {
            iu = 0;
            ldwrku = M;
            ir = iu + ldwrku * M;
            ldwrkr = M;
            itau = ir + ldwrkr * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "upper",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dorglq(N, N, M, VT, svt1, svt2, offsetVT, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrku,
              iu
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrku,
              iu + ldwrku
            );
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dlacpy(
              "lower",
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              WK,
              1,
              ldwrkr,
              ir
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrku, iu, WK, 1, itaup, WK, 1, iwork);
            dorgbr("apply-Q", M, M, M, WK, 1, ldwrkr, ir, WK, 1, itauq, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              M,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrku,
              iu,
              WK,
              1,
              ldwrkr,
              ir,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrku,
              iu,
              VT,
              svt1,
              svt2,
              offsetVT,
              0,
              A,
              sa1,
              sa2,
              offsetA
            );
            dlacpy(
              "full",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dlacpy(
              "full",
              M,
              M,
              WK,
              1,
              ldwrkr,
              ir,
              A,
              sa1,
              sa2,
              offsetA
            );
          } else if (wntuas) {
            iu = 0;
            ldwrku = M;
            itau = iu + ldwrku * M;
            iwork = itau + M;
            dgelqf(M, N, A, sa1, sa2, offsetA, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "upper",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
            dorglq(N, N, M, VT, svt1, svt2, offsetVT, WK, 1, itau, WK, 1, iwork);
            dlacpy(
              "lower",
              M,
              M,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              ldwrku,
              iu
            );
            dlaset(
              "upper",
              M - 1,
              M - 1,
              0,
              0,
              WK,
              1,
              ldwrku,
              iu + ldwrku
            );
            ie = itau;
            itauq = ie + M;
            itaup = itauq + M;
            iwork = itaup + M;
            dgebrd(
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              itauq,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              wsz - iwork
            );
            dlacpy(
              "lower",
              M,
              M,
              WK,
              1,
              ldwrku,
              iu,
              U,
              su1,
              su2,
              offsetU
            );
            dorgbr("apply-P", M, M, M, WK, 1, ldwrku, iu, WK, 1, itaup, WK, 1, iwork);
            dorgbr("apply-Q", M, M, M, U, su1, su2, offsetU, WK, 1, itauq, WK, 1, iwork);
            iwork = ie + M;
            info = dbdsqr(
              "upper",
              M,
              M,
              M,
              0,
              s,
              strideS,
              offsetS,
              WK,
              1,
              ie,
              WK,
              1,
              ldwrku,
              iu,
              U,
              su1,
              su2,
              offsetU,
              DUM,
              1,
              1,
              0,
              WK,
              1,
              iwork
            );
            dgemm2(
              "no-transpose",
              "no-transpose",
              M,
              N,
              M,
              1,
              WK,
              1,
              ldwrku,
              iu,
              VT,
              svt1,
              svt2,
              offsetVT,
              0,
              A,
              sa1,
              sa2,
              offsetA
            );
            dlacpy(
              "full",
              M,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              VT,
              svt1,
              svt2,
              offsetVT
            );
          }
        }
      } else {
        ie = 0;
        itauq = ie + M;
        itaup = itauq + M;
        iwork = itaup + M;
        dgebrd(
          M,
          N,
          A,
          sa1,
          sa2,
          offsetA,
          s,
          strideS,
          offsetS,
          WK,
          1,
          ie,
          WK,
          1,
          itauq,
          WK,
          1,
          itaup,
          WK,
          1,
          iwork,
          wsz - iwork
        );
        if (wntuas) {
          dlacpy(
            "lower",
            M,
            M,
            A,
            sa1,
            sa2,
            offsetA,
            U,
            su1,
            su2,
            offsetU
          );
          dorgbr("apply-Q", M, M, N, U, su1, su2, offsetU, WK, 1, itauq, WK, 1, iwork);
        }
        if (wntvas) {
          dlacpy(
            "upper",
            M,
            N,
            A,
            sa1,
            sa2,
            offsetA,
            VT,
            svt1,
            svt2,
            offsetVT
          );
          nrvt = wntva ? N : M;
          dorgbr("apply-P", nrvt, N, M, VT, svt1, svt2, offsetVT, WK, 1, itaup, WK, 1, iwork);
        }
        if (wntuo) {
          dorgbr("apply-Q", M, M, N, A, sa1, sa2, offsetA, WK, 1, itauq, WK, 1, iwork);
        }
        if (wntvo) {
          dorgbr("apply-P", M, N, M, A, sa1, sa2, offsetA, WK, 1, itaup, WK, 1, iwork);
        }
        iwork = ie + M;
        nru = 0;
        ncvt = 0;
        if (wntuas || wntuo) {
          nru = M;
        }
        if (wntvas || wntvo) {
          ncvt = N;
        }
        if (!wntuo && !wntvo) {
          info = dbdsqr(
            "lower",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            VT,
            svt1,
            svt2,
            offsetVT,
            U,
            su1,
            su2,
            offsetU,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
        } else if (!wntuo && wntvo) {
          info = dbdsqr(
            "lower",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            A,
            sa1,
            sa2,
            offsetA,
            U,
            su1,
            su2,
            offsetU,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
        } else {
          info = dbdsqr(
            "lower",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            WK,
            1,
            ie,
            VT,
            svt1,
            svt2,
            offsetVT,
            A,
            sa1,
            sa2,
            offsetA,
            DUM,
            1,
            1,
            0,
            WK,
            1,
            iwork
          );
        }
      }
      if (iscl === 1) {
        if (anrm > bignum) {
          dlascl(
            "general",
            0,
            0,
            bignum,
            anrm,
            minmn,
            1,
            s,
            strideS,
            1,
            offsetS
          );
        }
        if (info !== 0 && anrm > bignum) {
          dlascl(
            "general",
            0,
            0,
            bignum,
            anrm,
            minmn - 1,
            1,
            WK,
            1,
            1,
            ie
          );
        }
        if (anrm < smlnum) {
          dlascl(
            "general",
            0,
            0,
            smlnum,
            anrm,
            minmn,
            1,
            s,
            strideS,
            1,
            offsetS
          );
        }
        if (info !== 0 && anrm < smlnum) {
          dlascl(
            "general",
            0,
            0,
            smlnum,
            anrm,
            minmn - 1,
            1,
            WK,
            1,
            1,
            ie
          );
        }
      }
      return info;
    }
    module.exports = dgesvd2;
  }
});

// notebooks/pseudospectra/pspectra-entry.js
var import_base = __toESM(require_base55(), 1);
var import_base2 = __toESM(require_base78(), 1);
var import_base3 = __toESM(require_base22(), 1);
var export_dgeev = import_base.default;
var export_dgemm = import_base3.default;
var export_dgesvd = import_base2.default;
export {
  export_dgeev as dgeev,
  export_dgemm as dgemm,
  export_dgesvd as dgesvd
};
/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
/*! Bundled license information:

@stdlib/assert/has-symbol-support/lib/main.js:
@stdlib/assert/has-symbol-support/lib/index.js:
@stdlib/assert/has-tostringtag-support/lib/main.js:
@stdlib/assert/has-tostringtag-support/lib/index.js:
@stdlib/utils/native-class/lib/tostring.js:
@stdlib/utils/native-class/lib/main.js:
@stdlib/assert/has-own-property/lib/main.js:
@stdlib/assert/has-own-property/lib/index.js:
@stdlib/symbol/ctor/lib/main.js:
@stdlib/symbol/ctor/lib/index.js:
@stdlib/utils/native-class/lib/tostringtag.js:
@stdlib/utils/native-class/lib/polyfill.js:
@stdlib/utils/native-class/lib/index.js:
@stdlib/assert/is-float64array/lib/main.js:
@stdlib/assert/is-float64array/lib/index.js:
@stdlib/assert/has-float64array-support/lib/float64array.js:
@stdlib/assert/has-float64array-support/lib/main.js:
@stdlib/assert/has-float64array-support/lib/index.js:
@stdlib/array/float64/lib/main.js:
@stdlib/array/float64/lib/polyfill.js:
@stdlib/array/float64/lib/index.js:
  (**
  * @license Apache-2.0
  *
  * Copyright (c) 2018 The Stdlib Authors.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *    http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  *)
*/
