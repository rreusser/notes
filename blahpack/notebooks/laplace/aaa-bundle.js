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

// node_modules/@stdlib/utils/define-property/lib/define_property.js
var require_define_property = __commonJS({
  "node_modules/@stdlib/utils/define-property/lib/define_property.js"(exports, module) {
    "use strict";
    var main = typeof Object.defineProperty === "function" ? Object.defineProperty : null;
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/define-property/lib/has_define_property_support.js
var require_has_define_property_support = __commonJS({
  "node_modules/@stdlib/utils/define-property/lib/has_define_property_support.js"(exports, module) {
    "use strict";
    var defineProperty = require_define_property();
    function hasDefinePropertySupport() {
      try {
        defineProperty({}, "x", {});
        return true;
      } catch (err) {
        return false;
      }
    }
    module.exports = hasDefinePropertySupport;
  }
});

// node_modules/@stdlib/utils/define-property/lib/builtin.js
var require_builtin = __commonJS({
  "node_modules/@stdlib/utils/define-property/lib/builtin.js"(exports, module) {
    "use strict";
    var defineProperty = Object.defineProperty;
    module.exports = defineProperty;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/is_number.js
var require_is_number = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/is_number.js"(exports, module) {
    "use strict";
    function isNumber(value) {
      return typeof value === "number";
    }
    module.exports = isNumber;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/zero_pad.js
var require_zero_pad = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/zero_pad.js"(exports, module) {
    "use strict";
    function startsWithMinus(str) {
      return str[0] === "-";
    }
    function zeros(n) {
      var out = "";
      var i;
      for (i = 0; i < n; i++) {
        out += "0";
      }
      return out;
    }
    function zeroPad(str, width, right) {
      var negative = false;
      var pad = width - str.length;
      if (pad < 0) {
        return str;
      }
      if (startsWithMinus(str)) {
        negative = true;
        str = str.substr(1);
      }
      str = right ? str + zeros(pad) : zeros(pad) + str;
      if (negative) {
        str = "-" + str;
      }
      return str;
    }
    module.exports = zeroPad;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/format_integer.js
var require_format_integer = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/format_integer.js"(exports, module) {
    "use strict";
    var isNumber = require_is_number();
    var zeroPad = require_zero_pad();
    var lowercase = String.prototype.toLowerCase;
    var uppercase = String.prototype.toUpperCase;
    function formatInteger(token) {
      var base;
      var out;
      var i;
      switch (token.specifier) {
        case "b":
          base = 2;
          break;
        case "o":
          base = 8;
          break;
        case "x":
        case "X":
          base = 16;
          break;
        case "d":
        case "i":
        case "u":
        default:
          base = 10;
          break;
      }
      out = token.arg;
      i = parseInt(out, 10);
      if (!isFinite(i)) {
        if (!isNumber(out)) {
          throw new Error("invalid integer. Value: " + out);
        }
        i = 0;
      }
      if (i < 0 && (token.specifier === "u" || base !== 10)) {
        i = 4294967295 + i + 1;
      }
      if (i < 0) {
        out = (-i).toString(base);
        if (token.precision) {
          out = zeroPad(out, token.precision, token.padRight);
        }
        out = "-" + out;
      } else {
        out = i.toString(base);
        if (!i && !token.precision) {
          out = "";
        } else if (token.precision) {
          out = zeroPad(out, token.precision, token.padRight);
        }
        if (token.sign) {
          out = token.sign + out;
        }
      }
      if (base === 16) {
        if (token.alternate) {
          out = "0x" + out;
        }
        out = token.specifier === uppercase.call(token.specifier) ? uppercase.call(out) : lowercase.call(out);
      }
      if (base === 8) {
        if (token.alternate && out.charAt(0) !== "0") {
          out = "0" + out;
        }
      }
      return out;
    }
    module.exports = formatInteger;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/is_string.js
var require_is_string = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/is_string.js"(exports, module) {
    "use strict";
    function isString(value) {
      return typeof value === "string";
    }
    module.exports = isString;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/format_double.js
var require_format_double = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/format_double.js"(exports, module) {
    "use strict";
    var isNumber = require_is_number();
    var abs = Math.abs;
    var lowercase = String.prototype.toLowerCase;
    var uppercase = String.prototype.toUpperCase;
    var replace = String.prototype.replace;
    var RE_EXP_POS_DIGITS = /e\+(\d)$/;
    var RE_EXP_NEG_DIGITS = /e-(\d)$/;
    var RE_ONLY_DIGITS = /^(\d+)$/;
    var RE_DIGITS_BEFORE_EXP = /^(\d+)e/;
    var RE_TRAILING_PERIOD_ZERO = /\.0$/;
    var RE_PERIOD_ZERO_EXP = /\.0*e/;
    var RE_ZERO_BEFORE_EXP = /(\..*[^0])0*e/;
    function formatDouble(token) {
      var digits;
      var out;
      var f = parseFloat(token.arg);
      if (!isFinite(f)) {
        if (!isNumber(token.arg)) {
          throw new Error("invalid floating-point number. Value: " + out);
        }
        f = token.arg;
      }
      switch (token.specifier) {
        case "e":
        case "E":
          out = f.toExponential(token.precision);
          break;
        case "f":
        case "F":
          out = f.toFixed(token.precision);
          break;
        case "g":
        case "G":
          if (abs(f) < 1e-4) {
            digits = token.precision;
            if (digits > 0) {
              digits -= 1;
            }
            out = f.toExponential(digits);
          } else {
            out = f.toPrecision(token.precision);
          }
          if (!token.alternate) {
            out = replace.call(out, RE_ZERO_BEFORE_EXP, "$1e");
            out = replace.call(out, RE_PERIOD_ZERO_EXP, "e");
            out = replace.call(out, RE_TRAILING_PERIOD_ZERO, "");
          }
          break;
        default:
          throw new Error("invalid double notation. Value: " + token.specifier);
      }
      out = replace.call(out, RE_EXP_POS_DIGITS, "e+0$1");
      out = replace.call(out, RE_EXP_NEG_DIGITS, "e-0$1");
      if (token.alternate) {
        out = replace.call(out, RE_ONLY_DIGITS, "$1.");
        out = replace.call(out, RE_DIGITS_BEFORE_EXP, "$1.e");
      }
      if (f >= 0 && token.sign) {
        out = token.sign + out;
      }
      out = token.specifier === uppercase.call(token.specifier) ? uppercase.call(out) : lowercase.call(out);
      return out;
    }
    module.exports = formatDouble;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/space_pad.js
var require_space_pad = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/space_pad.js"(exports, module) {
    "use strict";
    function spaces(n) {
      var out = "";
      var i;
      for (i = 0; i < n; i++) {
        out += " ";
      }
      return out;
    }
    function spacePad(str, width, right) {
      var pad = width - str.length;
      if (pad < 0) {
        return str;
      }
      str = right ? str + spaces(pad) : spaces(pad) + str;
      return str;
    }
    module.exports = spacePad;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/main.js
var require_main = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/main.js"(exports, module) {
    "use strict";
    var formatInteger = require_format_integer();
    var isString = require_is_string();
    var formatDouble = require_format_double();
    var spacePad = require_space_pad();
    var zeroPad = require_zero_pad();
    var fromCharCode = String.fromCharCode;
    var isArray = Array.isArray;
    function isnan(value) {
      return value !== value;
    }
    function initialize(token) {
      var out = {};
      out.specifier = token.specifier;
      out.precision = token.precision === void 0 ? 1 : token.precision;
      out.width = token.width;
      out.flags = token.flags || "";
      out.mapping = token.mapping;
      return out;
    }
    function formatInterpolate(tokens) {
      var hasPeriod;
      var flags;
      var token;
      var flag;
      var num;
      var out;
      var pos;
      var i;
      var j;
      if (!isArray(tokens)) {
        throw new TypeError("invalid argument. First argument must be an array. Value: `" + tokens + "`.");
      }
      out = "";
      pos = 1;
      for (i = 0; i < tokens.length; i++) {
        token = tokens[i];
        if (isString(token)) {
          out += token;
        } else {
          hasPeriod = token.precision !== void 0;
          token = initialize(token);
          if (!token.specifier) {
            throw new TypeError("invalid argument. Token is missing `specifier` property. Index: `" + i + "`. Value: `" + token + "`.");
          }
          if (token.mapping) {
            pos = token.mapping;
          }
          flags = token.flags;
          for (j = 0; j < flags.length; j++) {
            flag = flags.charAt(j);
            switch (flag) {
              case " ":
                token.sign = " ";
                break;
              case "+":
                token.sign = "+";
                break;
              case "-":
                token.padRight = true;
                token.padZeros = false;
                break;
              case "0":
                token.padZeros = flags.indexOf("-") < 0;
                break;
              case "#":
                token.alternate = true;
                break;
              default:
                throw new Error("invalid flag: " + flag);
            }
          }
          if (token.width === "*") {
            token.width = parseInt(arguments[pos], 10);
            pos += 1;
            if (isnan(token.width)) {
              throw new TypeError("the argument for * width at position " + pos + " is not a number. Value: `" + token.width + "`.");
            }
            if (token.width < 0) {
              token.padRight = true;
              token.width = -token.width;
            }
          }
          if (hasPeriod) {
            if (token.precision === "*") {
              token.precision = parseInt(arguments[pos], 10);
              pos += 1;
              if (isnan(token.precision)) {
                throw new TypeError("the argument for * precision at position " + pos + " is not a number. Value: `" + token.precision + "`.");
              }
              if (token.precision < 0) {
                token.precision = 1;
                hasPeriod = false;
              }
            }
          }
          token.arg = arguments[pos];
          switch (token.specifier) {
            case "b":
            case "o":
            case "x":
            case "X":
            case "d":
            case "i":
            case "u":
              if (hasPeriod) {
                token.padZeros = false;
              }
              token.arg = formatInteger(token);
              break;
            case "s":
              token.maxWidth = hasPeriod ? token.precision : -1;
              token.arg = String(token.arg);
              break;
            case "c":
              if (!isnan(token.arg)) {
                num = parseInt(token.arg, 10);
                if (num < 0 || num > 127) {
                  throw new Error("invalid character code. Value: " + token.arg);
                }
                token.arg = isnan(num) ? String(token.arg) : fromCharCode(num);
              }
              break;
            case "e":
            case "E":
            case "f":
            case "F":
            case "g":
            case "G":
              if (!hasPeriod) {
                token.precision = 6;
              }
              token.arg = formatDouble(token);
              break;
            default:
              throw new Error("invalid specifier: " + token.specifier);
          }
          if (token.maxWidth >= 0 && token.arg.length > token.maxWidth) {
            token.arg = token.arg.substring(0, token.maxWidth);
          }
          if (token.padZeros) {
            token.arg = zeroPad(token.arg, token.width || token.precision, token.padRight);
          } else if (token.width) {
            token.arg = spacePad(token.arg, token.width, token.padRight);
          }
          out += token.arg || "";
          pos += 1;
        }
      }
      return out;
    }
    module.exports = formatInterpolate;
  }
});

// node_modules/@stdlib/string/base/format-interpolate/lib/index.js
var require_lib = __commonJS({
  "node_modules/@stdlib/string/base/format-interpolate/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main();
    module.exports = main;
  }
});

// node_modules/@stdlib/string/base/format-tokenize/lib/main.js
var require_main2 = __commonJS({
  "node_modules/@stdlib/string/base/format-tokenize/lib/main.js"(exports, module) {
    "use strict";
    var RE = /%(?:([1-9]\d*)\$)?([0 +\-#]*)(\*|\d+)?(?:(\.)(\*|\d+)?)?[hlL]?([%A-Za-z])/g;
    function parse(match) {
      var token = {
        "mapping": match[1] ? parseInt(match[1], 10) : void 0,
        "flags": match[2],
        "width": match[3],
        "precision": match[5],
        "specifier": match[6]
      };
      if (match[4] === "." && match[5] === void 0) {
        token.precision = "1";
      }
      return token;
    }
    function formatTokenize(str) {
      var content;
      var tokens;
      var match;
      var prev;
      tokens = [];
      prev = 0;
      match = RE.exec(str);
      while (match) {
        content = str.slice(prev, RE.lastIndex - match[0].length);
        if (content.length) {
          tokens.push(content);
        }
        tokens.push(parse(match));
        prev = RE.lastIndex;
        match = RE.exec(str);
      }
      content = str.slice(prev);
      if (content.length) {
        tokens.push(content);
      }
      return tokens;
    }
    module.exports = formatTokenize;
  }
});

// node_modules/@stdlib/string/base/format-tokenize/lib/index.js
var require_lib2 = __commonJS({
  "node_modules/@stdlib/string/base/format-tokenize/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main2();
    module.exports = main;
  }
});

// node_modules/@stdlib/string/format/lib/is_string.js
var require_is_string2 = __commonJS({
  "node_modules/@stdlib/string/format/lib/is_string.js"(exports, module) {
    "use strict";
    function isString(value) {
      return typeof value === "string";
    }
    module.exports = isString;
  }
});

// node_modules/@stdlib/string/format/lib/main.js
var require_main3 = __commonJS({
  "node_modules/@stdlib/string/format/lib/main.js"(exports, module) {
    "use strict";
    var interpolate = require_lib();
    var tokenize = require_lib2();
    var isString = require_is_string2();
    function format(str) {
      var args;
      var i;
      if (!isString(str)) {
        throw new TypeError(format("invalid argument. First argument must be a string. Value: `%s`.", str));
      }
      args = [tokenize(str)];
      for (i = 1; i < arguments.length; i++) {
        args.push(arguments[i]);
      }
      return interpolate.apply(null, args);
    }
    module.exports = format;
  }
});

// node_modules/@stdlib/string/format/lib/index.js
var require_lib3 = __commonJS({
  "node_modules/@stdlib/string/format/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main3();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/define-property/lib/polyfill.js
var require_polyfill = __commonJS({
  "node_modules/@stdlib/utils/define-property/lib/polyfill.js"(exports, module) {
    "use strict";
    var format = require_lib3();
    var objectProtoype = Object.prototype;
    var toStr = objectProtoype.toString;
    var defineGetter = objectProtoype.__defineGetter__;
    var defineSetter = objectProtoype.__defineSetter__;
    var lookupGetter = objectProtoype.__lookupGetter__;
    var lookupSetter = objectProtoype.__lookupSetter__;
    function defineProperty(obj, prop, descriptor) {
      var prototype;
      var hasValue;
      var hasGet;
      var hasSet;
      if (typeof obj !== "object" || obj === null || toStr.call(obj) === "[object Array]") {
        throw new TypeError(format("invalid argument. First argument must be an object. Value: `%s`.", obj));
      }
      if (typeof descriptor !== "object" || descriptor === null || toStr.call(descriptor) === "[object Array]") {
        throw new TypeError(format("invalid argument. Property descriptor must be an object. Value: `%s`.", descriptor));
      }
      hasValue = "value" in descriptor;
      if (hasValue) {
        if (lookupGetter.call(obj, prop) || lookupSetter.call(obj, prop)) {
          prototype = obj.__proto__;
          obj.__proto__ = objectProtoype;
          delete obj[prop];
          obj[prop] = descriptor.value;
          obj.__proto__ = prototype;
        } else {
          obj[prop] = descriptor.value;
        }
      }
      hasGet = "get" in descriptor;
      hasSet = "set" in descriptor;
      if (hasValue && (hasGet || hasSet)) {
        throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");
      }
      if (hasGet && defineGetter) {
        defineGetter.call(obj, prop, descriptor.get);
      }
      if (hasSet && defineSetter) {
        defineSetter.call(obj, prop, descriptor.set);
      }
      return obj;
    }
    module.exports = defineProperty;
  }
});

// node_modules/@stdlib/utils/define-property/lib/index.js
var require_lib4 = __commonJS({
  "node_modules/@stdlib/utils/define-property/lib/index.js"(exports, module) {
    "use strict";
    var hasDefinePropertySupport = require_has_define_property_support();
    var builtin = require_builtin();
    var polyfill = require_polyfill();
    var defineProperty;
    if (hasDefinePropertySupport()) {
      defineProperty = builtin;
    } else {
      defineProperty = polyfill;
    }
    module.exports = defineProperty;
  }
});

// node_modules/@stdlib/utils/define-nonenumerable-read-only-property/lib/main.js
var require_main4 = __commonJS({
  "node_modules/@stdlib/utils/define-nonenumerable-read-only-property/lib/main.js"(exports, module) {
    "use strict";
    var defineProperty = require_lib4();
    function setNonEnumerableReadOnly(obj, prop, value) {
      defineProperty(obj, prop, {
        "configurable": false,
        "enumerable": false,
        "writable": false,
        "value": value
      });
    }
    module.exports = setNonEnumerableReadOnly;
  }
});

// node_modules/@stdlib/utils/define-nonenumerable-read-only-property/lib/index.js
var require_lib5 = __commonJS({
  "node_modules/@stdlib/utils/define-nonenumerable-read-only-property/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main4();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-number/lib/primitive.js
var require_primitive = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/primitive.js"(exports, module) {
    "use strict";
    function isNumber(value) {
      return typeof value === "number";
    }
    module.exports = isNumber;
  }
});

// node_modules/@stdlib/assert/has-symbol-support/lib/main.js
var require_main5 = __commonJS({
  "node_modules/@stdlib/assert/has-symbol-support/lib/main.js"(exports, module) {
    "use strict";
    function hasSymbolSupport() {
      return typeof Symbol === "function" && typeof /* @__PURE__ */ Symbol("foo") === "symbol";
    }
    module.exports = hasSymbolSupport;
  }
});

// node_modules/@stdlib/assert/has-symbol-support/lib/index.js
var require_lib6 = __commonJS({
  "node_modules/@stdlib/assert/has-symbol-support/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main5();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/has-tostringtag-support/lib/main.js
var require_main6 = __commonJS({
  "node_modules/@stdlib/assert/has-tostringtag-support/lib/main.js"(exports, module) {
    "use strict";
    var hasSymbols = require_lib6();
    var FLG = hasSymbols();
    function hasToStringTagSupport() {
      return FLG && typeof Symbol.toStringTag === "symbol";
    }
    module.exports = hasToStringTagSupport;
  }
});

// node_modules/@stdlib/assert/has-tostringtag-support/lib/index.js
var require_lib7 = __commonJS({
  "node_modules/@stdlib/assert/has-tostringtag-support/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main6();
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
var require_main7 = __commonJS({
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
var require_main8 = __commonJS({
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
var require_lib8 = __commonJS({
  "node_modules/@stdlib/assert/has-own-property/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main8();
    module.exports = main;
  }
});

// node_modules/@stdlib/symbol/ctor/lib/main.js
var require_main9 = __commonJS({
  "node_modules/@stdlib/symbol/ctor/lib/main.js"(exports, module) {
    "use strict";
    var Sym = typeof Symbol === "function" ? Symbol : void 0;
    module.exports = Sym;
  }
});

// node_modules/@stdlib/symbol/ctor/lib/index.js
var require_lib9 = __commonJS({
  "node_modules/@stdlib/symbol/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main9();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/native-class/lib/tostringtag.js
var require_tostringtag = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/tostringtag.js"(exports, module) {
    "use strict";
    var Symbol2 = require_lib9();
    var toStrTag = typeof Symbol2 === "function" ? Symbol2.toStringTag : "";
    module.exports = toStrTag;
  }
});

// node_modules/@stdlib/utils/native-class/lib/polyfill.js
var require_polyfill2 = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/polyfill.js"(exports, module) {
    "use strict";
    var hasOwnProp = require_lib8();
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
var require_lib10 = __commonJS({
  "node_modules/@stdlib/utils/native-class/lib/index.js"(exports, module) {
    "use strict";
    var hasToStringTag = require_lib7();
    var builtin = require_main7();
    var polyfill = require_polyfill2();
    var main;
    if (hasToStringTag()) {
      main = polyfill;
    } else {
      main = builtin;
    }
    module.exports = main;
  }
});

// node_modules/@stdlib/number/ctor/lib/main.js
var require_main10 = __commonJS({
  "node_modules/@stdlib/number/ctor/lib/main.js"(exports, module) {
    "use strict";
    module.exports = Number;
  }
});

// node_modules/@stdlib/number/ctor/lib/index.js
var require_lib11 = __commonJS({
  "node_modules/@stdlib/number/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main10();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-number/lib/tostring.js
var require_tostring2 = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/tostring.js"(exports, module) {
    "use strict";
    var Number2 = require_lib11();
    var toString = Number2.prototype.toString;
    module.exports = toString;
  }
});

// node_modules/@stdlib/assert/is-number/lib/try2serialize.js
var require_try2serialize = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/try2serialize.js"(exports, module) {
    "use strict";
    var toString = require_tostring2();
    function test(value) {
      try {
        toString.call(value);
        return true;
      } catch (err) {
        return false;
      }
    }
    module.exports = test;
  }
});

// node_modules/@stdlib/assert/is-number/lib/object.js
var require_object = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/object.js"(exports, module) {
    "use strict";
    var hasToStringTag = require_lib7();
    var nativeClass = require_lib10();
    var Number2 = require_lib11();
    var test = require_try2serialize();
    var FLG = hasToStringTag();
    function isNumber(value) {
      if (typeof value === "object") {
        if (value instanceof Number2) {
          return true;
        }
        if (FLG) {
          return test(value);
        }
        return nativeClass(value) === "[object Number]";
      }
      return false;
    }
    module.exports = isNumber;
  }
});

// node_modules/@stdlib/assert/is-number/lib/main.js
var require_main11 = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/main.js"(exports, module) {
    "use strict";
    var isPrimitive = require_primitive();
    var isObject = require_object();
    function isNumber(value) {
      return isPrimitive(value) || isObject(value);
    }
    module.exports = isNumber;
  }
});

// node_modules/@stdlib/assert/is-number/lib/index.js
var require_lib12 = __commonJS({
  "node_modules/@stdlib/assert/is-number/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main11();
    var isPrimitive = require_primitive();
    var isObject = require_object();
    setReadOnly(main, "isPrimitive", isPrimitive);
    setReadOnly(main, "isObject", isObject);
    module.exports = main;
  }
});

// node_modules/@stdlib/constants/float64/pinf/lib/index.js
var require_lib13 = __commonJS({
  "node_modules/@stdlib/constants/float64/pinf/lib/index.js"(exports, module) {
    "use strict";
    var FLOAT64_PINF = Number.POSITIVE_INFINITY;
    module.exports = FLOAT64_PINF;
  }
});

// node_modules/@stdlib/constants/float64/ninf/lib/index.js
var require_lib14 = __commonJS({
  "node_modules/@stdlib/constants/float64/ninf/lib/index.js"(exports, module) {
    "use strict";
    var Number2 = require_lib11();
    var FLOAT64_NINF = Number2.NEGATIVE_INFINITY;
    module.exports = FLOAT64_NINF;
  }
});

// node_modules/@stdlib/math/base/special/floor/lib/main.js
var require_main12 = __commonJS({
  "node_modules/@stdlib/math/base/special/floor/lib/main.js"(exports, module) {
    "use strict";
    var floor = Math.floor;
    module.exports = floor;
  }
});

// node_modules/@stdlib/math/base/special/floor/lib/index.js
var require_lib15 = __commonJS({
  "node_modules/@stdlib/math/base/special/floor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main12();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/assert/is-integer/lib/main.js
var require_main13 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-integer/lib/main.js"(exports, module) {
    "use strict";
    var floor = require_lib15();
    function isInteger(x) {
      return floor(x) === x;
    }
    module.exports = isInteger;
  }
});

// node_modules/@stdlib/math/base/assert/is-integer/lib/index.js
var require_lib16 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-integer/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main13();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-integer/lib/integer.js
var require_integer = __commonJS({
  "node_modules/@stdlib/assert/is-integer/lib/integer.js"(exports, module) {
    "use strict";
    var PINF = require_lib13();
    var NINF = require_lib14();
    var isInt = require_lib16();
    function isInteger(value) {
      return value < PINF && value > NINF && isInt(value);
    }
    module.exports = isInteger;
  }
});

// node_modules/@stdlib/assert/is-integer/lib/primitive.js
var require_primitive2 = __commonJS({
  "node_modules/@stdlib/assert/is-integer/lib/primitive.js"(exports, module) {
    "use strict";
    var isNumber = require_lib12().isPrimitive;
    var isInt = require_integer();
    function isInteger(value) {
      return isNumber(value) && isInt(value);
    }
    module.exports = isInteger;
  }
});

// node_modules/@stdlib/assert/is-integer/lib/object.js
var require_object2 = __commonJS({
  "node_modules/@stdlib/assert/is-integer/lib/object.js"(exports, module) {
    "use strict";
    var isNumber = require_lib12().isObject;
    var isInt = require_integer();
    function isInteger(value) {
      return isNumber(value) && isInt(value.valueOf());
    }
    module.exports = isInteger;
  }
});

// node_modules/@stdlib/assert/is-integer/lib/main.js
var require_main14 = __commonJS({
  "node_modules/@stdlib/assert/is-integer/lib/main.js"(exports, module) {
    "use strict";
    var isPrimitive = require_primitive2();
    var isObject = require_object2();
    function isInteger(value) {
      return isPrimitive(value) || isObject(value);
    }
    module.exports = isInteger;
  }
});

// node_modules/@stdlib/assert/is-integer/lib/index.js
var require_lib17 = __commonJS({
  "node_modules/@stdlib/assert/is-integer/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main14();
    var isPrimitive = require_primitive2();
    var isObject = require_object2();
    setReadOnly(main, "isPrimitive", isPrimitive);
    setReadOnly(main, "isObject", isObject);
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-nonnegative-integer/lib/primitive.js
var require_primitive3 = __commonJS({
  "node_modules/@stdlib/assert/is-nonnegative-integer/lib/primitive.js"(exports, module) {
    "use strict";
    var isInteger = require_lib17().isPrimitive;
    function isNonNegativeInteger(value) {
      return isInteger(value) && value >= 0;
    }
    module.exports = isNonNegativeInteger;
  }
});

// node_modules/@stdlib/assert/is-nonnegative-integer/lib/object.js
var require_object3 = __commonJS({
  "node_modules/@stdlib/assert/is-nonnegative-integer/lib/object.js"(exports, module) {
    "use strict";
    var isInteger = require_lib17().isObject;
    function isNonNegativeInteger(value) {
      return isInteger(value) && value.valueOf() >= 0;
    }
    module.exports = isNonNegativeInteger;
  }
});

// node_modules/@stdlib/assert/is-nonnegative-integer/lib/main.js
var require_main15 = __commonJS({
  "node_modules/@stdlib/assert/is-nonnegative-integer/lib/main.js"(exports, module) {
    "use strict";
    var isPrimitive = require_primitive3();
    var isObject = require_object3();
    function isNonNegativeInteger(value) {
      return isPrimitive(value) || isObject(value);
    }
    module.exports = isNonNegativeInteger;
  }
});

// node_modules/@stdlib/assert/is-nonnegative-integer/lib/index.js
var require_lib18 = __commonJS({
  "node_modules/@stdlib/assert/is-nonnegative-integer/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main15();
    var isPrimitive = require_primitive3();
    var isObject = require_object3();
    setReadOnly(main, "isPrimitive", isPrimitive);
    setReadOnly(main, "isObject", isObject);
    module.exports = main;
  }
});

// node_modules/@stdlib/constants/array/max-array-length/lib/index.js
var require_lib19 = __commonJS({
  "node_modules/@stdlib/constants/array/max-array-length/lib/index.js"(exports, module) {
    "use strict";
    var MAX_ARRAY_LENGTH = 4294967295 >>> 0;
    module.exports = MAX_ARRAY_LENGTH;
  }
});

// node_modules/@stdlib/assert/is-array-like-object/lib/main.js
var require_main16 = __commonJS({
  "node_modules/@stdlib/assert/is-array-like-object/lib/main.js"(exports, module) {
    "use strict";
    var isInteger = require_lib16();
    var MAX_LENGTH = require_lib19();
    function isArrayLikeObject(value) {
      return typeof value === "object" && value !== null && typeof value.length === "number" && isInteger(value.length) && value.length >= 0 && value.length <= MAX_LENGTH;
    }
    module.exports = isArrayLikeObject;
  }
});

// node_modules/@stdlib/assert/is-array-like-object/lib/index.js
var require_lib20 = __commonJS({
  "node_modules/@stdlib/assert/is-array-like-object/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main16();
    module.exports = main;
  }
});

// node_modules/@stdlib/constants/array/max-typed-array-length/lib/index.js
var require_lib21 = __commonJS({
  "node_modules/@stdlib/constants/array/max-typed-array-length/lib/index.js"(exports, module) {
    "use strict";
    var MAX_TYPED_ARRAY_LENGTH = 9007199254740991;
    module.exports = MAX_TYPED_ARRAY_LENGTH;
  }
});

// node_modules/@stdlib/assert/is-collection/lib/main.js
var require_main17 = __commonJS({
  "node_modules/@stdlib/assert/is-collection/lib/main.js"(exports, module) {
    "use strict";
    var isInteger = require_lib16();
    var MAX_LENGTH = require_lib21();
    function isCollection(value) {
      return typeof value === "object" && value !== null && typeof value.length === "number" && isInteger(value.length) && value.length >= 0 && value.length <= MAX_LENGTH;
    }
    module.exports = isCollection;
  }
});

// node_modules/@stdlib/assert/is-collection/lib/index.js
var require_lib22 = __commonJS({
  "node_modules/@stdlib/assert/is-collection/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main17();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-arraybuffer/lib/main.js
var require_main18 = __commonJS({
  "node_modules/@stdlib/assert/is-arraybuffer/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib10();
    var hasArrayBuffer = typeof ArrayBuffer === "function";
    function isArrayBuffer(value) {
      return hasArrayBuffer && value instanceof ArrayBuffer || // eslint-disable-line stdlib/require-globals
      nativeClass(value) === "[object ArrayBuffer]";
    }
    module.exports = isArrayBuffer;
  }
});

// node_modules/@stdlib/assert/is-arraybuffer/lib/index.js
var require_lib23 = __commonJS({
  "node_modules/@stdlib/assert/is-arraybuffer/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main18();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-array/lib/main.js
var require_main19 = __commonJS({
  "node_modules/@stdlib/assert/is-array/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib10();
    var f;
    function isArray(value) {
      return nativeClass(value) === "[object Array]";
    }
    if (Array.isArray) {
      f = Array.isArray;
    } else {
      f = isArray;
    }
    module.exports = f;
  }
});

// node_modules/@stdlib/assert/is-array/lib/index.js
var require_lib24 = __commonJS({
  "node_modules/@stdlib/assert/is-array/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main19();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-object/lib/main.js
var require_main20 = __commonJS({
  "node_modules/@stdlib/assert/is-object/lib/main.js"(exports, module) {
    "use strict";
    var isArray = require_lib24();
    function isObject(value) {
      return typeof value === "object" && value !== null && !isArray(value);
    }
    module.exports = isObject;
  }
});

// node_modules/@stdlib/assert/is-object/lib/index.js
var require_lib25 = __commonJS({
  "node_modules/@stdlib/assert/is-object/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main20();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/tools/array-function/lib/main.js
var require_main21 = __commonJS({
  "node_modules/@stdlib/assert/tools/array-function/lib/main.js"(exports, module) {
    "use strict";
    var isArray = require_lib24();
    var format = require_lib3();
    function arrayfcn(predicate) {
      if (typeof predicate !== "function") {
        throw new TypeError(format("invalid argument. Must provide a function. Value: `%s`.", predicate));
      }
      return every;
      function every(value) {
        var len;
        var i;
        if (!isArray(value)) {
          return false;
        }
        len = value.length;
        if (len === 0) {
          return false;
        }
        for (i = 0; i < len; i++) {
          if (predicate(value[i]) === false) {
            return false;
          }
        }
        return true;
      }
    }
    module.exports = arrayfcn;
  }
});

// node_modules/@stdlib/assert/tools/array-function/lib/index.js
var require_lib26 = __commonJS({
  "node_modules/@stdlib/assert/tools/array-function/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main21();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-string/lib/primitive.js
var require_primitive4 = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/primitive.js"(exports, module) {
    "use strict";
    function isString(value) {
      return typeof value === "string";
    }
    module.exports = isString;
  }
});

// node_modules/@stdlib/assert/is-string/lib/valueof.js
var require_valueof = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/valueof.js"(exports, module) {
    "use strict";
    var valueOf = String.prototype.valueOf;
    module.exports = valueOf;
  }
});

// node_modules/@stdlib/assert/is-string/lib/try2valueof.js
var require_try2valueof = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/try2valueof.js"(exports, module) {
    "use strict";
    var valueOf = require_valueof();
    function test(value) {
      try {
        valueOf.call(value);
        return true;
      } catch (err) {
        return false;
      }
    }
    module.exports = test;
  }
});

// node_modules/@stdlib/assert/is-string/lib/object.js
var require_object4 = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/object.js"(exports, module) {
    "use strict";
    var hasToStringTag = require_lib7();
    var nativeClass = require_lib10();
    var test = require_try2valueof();
    var FLG = hasToStringTag();
    function isString(value) {
      if (typeof value === "object") {
        if (value instanceof String) {
          return true;
        }
        if (FLG) {
          return test(value);
        }
        return nativeClass(value) === "[object String]";
      }
      return false;
    }
    module.exports = isString;
  }
});

// node_modules/@stdlib/assert/is-string/lib/main.js
var require_main22 = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/main.js"(exports, module) {
    "use strict";
    var isPrimitive = require_primitive4();
    var isObject = require_object4();
    function isString(value) {
      return isPrimitive(value) || isObject(value);
    }
    module.exports = isString;
  }
});

// node_modules/@stdlib/assert/is-string/lib/index.js
var require_lib27 = __commonJS({
  "node_modules/@stdlib/assert/is-string/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main22();
    var isPrimitive = require_primitive4();
    var isObject = require_object4();
    setReadOnly(main, "isPrimitive", isPrimitive);
    setReadOnly(main, "isObject", isObject);
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-string-array/lib/index.js
var require_lib28 = __commonJS({
  "node_modules/@stdlib/assert/is-string-array/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var arrayfun = require_lib26();
    var isString = require_lib27();
    var isPrimitiveArray = arrayfun(isString.isPrimitive);
    var isObjectArray = arrayfun(isString.isObject);
    var isStringArray = arrayfun(isString);
    setReadOnly(isStringArray, "primitives", isPrimitiveArray);
    setReadOnly(isStringArray, "objects", isObjectArray);
    module.exports = isStringArray;
  }
});

// node_modules/@stdlib/utils/type-of/lib/fixtures/re.js
var require_re = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/fixtures/re.js"(exports, module) {
    "use strict";
    var RE = /./;
    module.exports = RE;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/primitive.js
var require_primitive5 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/primitive.js"(exports, module) {
    "use strict";
    function isBoolean(value) {
      return typeof value === "boolean";
    }
    module.exports = isBoolean;
  }
});

// node_modules/@stdlib/boolean/ctor/lib/main.js
var require_main23 = __commonJS({
  "node_modules/@stdlib/boolean/ctor/lib/main.js"(exports, module) {
    "use strict";
    var Bool = Boolean;
    module.exports = Bool;
  }
});

// node_modules/@stdlib/boolean/ctor/lib/index.js
var require_lib29 = __commonJS({
  "node_modules/@stdlib/boolean/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main23();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/tostring.js
var require_tostring3 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/tostring.js"(exports, module) {
    "use strict";
    var toString = Boolean.prototype.toString;
    module.exports = toString;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/try2serialize.js
var require_try2serialize2 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/try2serialize.js"(exports, module) {
    "use strict";
    var toString = require_tostring3();
    function test(value) {
      try {
        toString.call(value);
        return true;
      } catch (err) {
        return false;
      }
    }
    module.exports = test;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/object.js
var require_object5 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/object.js"(exports, module) {
    "use strict";
    var hasToStringTag = require_lib7();
    var nativeClass = require_lib10();
    var Boolean2 = require_lib29();
    var test = require_try2serialize2();
    var FLG = hasToStringTag();
    function isBoolean(value) {
      if (typeof value === "object") {
        if (value instanceof Boolean2) {
          return true;
        }
        if (FLG) {
          return test(value);
        }
        return nativeClass(value) === "[object Boolean]";
      }
      return false;
    }
    module.exports = isBoolean;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/main.js
var require_main24 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/main.js"(exports, module) {
    "use strict";
    var isPrimitive = require_primitive5();
    var isObject = require_object5();
    function isBoolean(value) {
      return isPrimitive(value) || isObject(value);
    }
    module.exports = isBoolean;
  }
});

// node_modules/@stdlib/assert/is-boolean/lib/index.js
var require_lib30 = __commonJS({
  "node_modules/@stdlib/assert/is-boolean/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main24();
    var isPrimitive = require_primitive5();
    var isObject = require_object5();
    setReadOnly(main, "isPrimitive", isPrimitive);
    setReadOnly(main, "isObject", isObject);
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/global/lib/codegen.js
var require_codegen = __commonJS({
  "node_modules/@stdlib/utils/global/lib/codegen.js"(exports, module) {
    "use strict";
    function getGlobal() {
      return new Function("return this;")();
    }
    module.exports = getGlobal;
  }
});

// node_modules/@stdlib/utils/global/lib/self.js
var require_self = __commonJS({
  "node_modules/@stdlib/utils/global/lib/self.js"(exports, module) {
    "use strict";
    var obj = typeof self === "object" ? self : null;
    module.exports = obj;
  }
});

// node_modules/@stdlib/utils/global/lib/window.js
var require_window = __commonJS({
  "node_modules/@stdlib/utils/global/lib/window.js"(exports, module) {
    "use strict";
    var obj = typeof window === "object" ? window : null;
    module.exports = obj;
  }
});

// node_modules/@stdlib/utils/global/lib/global.js
var require_global = __commonJS({
  "node_modules/@stdlib/utils/global/lib/global.js"(exports, module) {
    "use strict";
    var obj = typeof global === "object" ? global : null;
    module.exports = obj;
  }
});

// node_modules/@stdlib/utils/global/lib/global_this.js
var require_global_this = __commonJS({
  "node_modules/@stdlib/utils/global/lib/global_this.js"(exports, module) {
    "use strict";
    var obj = typeof globalThis === "object" ? globalThis : null;
    module.exports = obj;
  }
});

// node_modules/@stdlib/utils/global/lib/main.js
var require_main25 = __commonJS({
  "node_modules/@stdlib/utils/global/lib/main.js"(exports, module) {
    "use strict";
    var isBoolean = require_lib30().isPrimitive;
    var format = require_lib3();
    var getThis = require_codegen();
    var Self = require_self();
    var Win = require_window();
    var Global = require_global();
    var GlobalThis = require_global_this();
    function getGlobal(codegen) {
      if (arguments.length) {
        if (!isBoolean(codegen)) {
          throw new TypeError(format("invalid argument. Must provide a boolean. Value: `%s`.", codegen));
        }
        if (codegen) {
          return getThis();
        }
      }
      if (GlobalThis) {
        return GlobalThis;
      }
      if (Self) {
        return Self;
      }
      if (Win) {
        return Win;
      }
      if (Global) {
        return Global;
      }
      throw new Error("unexpected error. Unable to resolve global object.");
    }
    module.exports = getGlobal;
  }
});

// node_modules/@stdlib/utils/global/lib/index.js
var require_lib31 = __commonJS({
  "node_modules/@stdlib/utils/global/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main25();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/type-of/lib/fixtures/nodelist.js
var require_nodelist = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/fixtures/nodelist.js"(exports, module) {
    "use strict";
    var getGlobal = require_lib31();
    var root = getGlobal();
    var nodeList = root.document && root.document.childNodes;
    module.exports = nodeList;
  }
});

// node_modules/@stdlib/utils/type-of/lib/fixtures/typedarray.js
var require_typedarray = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/fixtures/typedarray.js"(exports, module) {
    "use strict";
    var typedarray = Int8Array;
    module.exports = typedarray;
  }
});

// node_modules/@stdlib/utils/type-of/lib/check.js
var require_check = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/check.js"(exports, module) {
    "use strict";
    var RE = require_re();
    var nodeList = require_nodelist();
    var typedarray = require_typedarray();
    function check() {
      if (
        // Chrome 1-12 returns 'function' for regular expression instances (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof):
        typeof RE === "function" || // Safari 8 returns 'object' for typed array and weak map constructors (underscore #1929):
        typeof typedarray === "object" || // PhantomJS 1.9 returns 'function' for `NodeList` instances (underscore #2236):
        typeof nodeList === "function"
      ) {
        return true;
      }
      return false;
    }
    module.exports = check;
  }
});

// node_modules/@stdlib/regexp/function-name/lib/main.js
var require_main26 = __commonJS({
  "node_modules/@stdlib/regexp/function-name/lib/main.js"(exports, module) {
    "use strict";
    function reFunctionName() {
      return /^\s*function\s*([^(]*)/i;
    }
    module.exports = reFunctionName;
  }
});

// node_modules/@stdlib/regexp/function-name/lib/regexp.js
var require_regexp = __commonJS({
  "node_modules/@stdlib/regexp/function-name/lib/regexp.js"(exports, module) {
    "use strict";
    var reFunctionName = require_main26();
    var RE_FUNCTION_NAME = reFunctionName();
    module.exports = RE_FUNCTION_NAME;
  }
});

// node_modules/@stdlib/regexp/function-name/lib/index.js
var require_lib32 = __commonJS({
  "node_modules/@stdlib/regexp/function-name/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var main = require_main26();
    var REGEXP = require_regexp();
    setReadOnly(main, "REGEXP", REGEXP);
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-object-like/lib/main.js
var require_main27 = __commonJS({
  "node_modules/@stdlib/assert/is-object-like/lib/main.js"(exports, module) {
    "use strict";
    function isObjectLike(value) {
      return value !== null && typeof value === "object";
    }
    module.exports = isObjectLike;
  }
});

// node_modules/@stdlib/assert/is-object-like/lib/index.js
var require_lib33 = __commonJS({
  "node_modules/@stdlib/assert/is-object-like/lib/index.js"(exports, module) {
    "use strict";
    var setReadOnly = require_lib5();
    var arrayfun = require_lib26();
    var main = require_main27();
    var isObjectLikeArray = arrayfun(main);
    setReadOnly(main, "isObjectLikeArray", isObjectLikeArray);
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-buffer/lib/main.js
var require_main28 = __commonJS({
  "node_modules/@stdlib/assert/is-buffer/lib/main.js"(exports, module) {
    "use strict";
    var isObjectLike = require_lib33();
    function isBuffer(value) {
      return isObjectLike(value) && // eslint-disable-next-line no-underscore-dangle
      (value._isBuffer || // for envs missing Object.prototype.constructor (e.g., Safari 5-7)
      value.constructor && // WARNING: `typeof` is not a foolproof check, as certain envs consider RegExp and NodeList instances to be functions
      typeof value.constructor.isBuffer === "function" && value.constructor.isBuffer(value));
    }
    module.exports = isBuffer;
  }
});

// node_modules/@stdlib/assert/is-buffer/lib/index.js
var require_lib34 = __commonJS({
  "node_modules/@stdlib/assert/is-buffer/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main28();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/constructor-name/lib/main.js
var require_main29 = __commonJS({
  "node_modules/@stdlib/utils/constructor-name/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib10();
    var RE = require_lib32().REGEXP;
    var isBuffer = require_lib34();
    function constructorName(v) {
      var match;
      var name;
      var ctor;
      name = nativeClass(v).slice(8, -1);
      if ((name === "Object" || name === "Error") && v.constructor) {
        ctor = v.constructor;
        if (typeof ctor.name === "string") {
          return ctor.name;
        }
        match = RE.exec(ctor.toString());
        if (match) {
          return match[1];
        }
      }
      if (isBuffer(v)) {
        return "Buffer";
      }
      return name;
    }
    module.exports = constructorName;
  }
});

// node_modules/@stdlib/utils/constructor-name/lib/index.js
var require_lib35 = __commonJS({
  "node_modules/@stdlib/utils/constructor-name/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main29();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/type-of/lib/main.js
var require_main30 = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/main.js"(exports, module) {
    "use strict";
    var ctorName = require_lib35();
    function typeOf(v) {
      var type;
      if (v === null) {
        return "null";
      }
      type = typeof v;
      if (type === "object") {
        return ctorName(v).toLowerCase();
      }
      return type;
    }
    module.exports = typeOf;
  }
});

// node_modules/@stdlib/utils/type-of/lib/polyfill.js
var require_polyfill3 = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/polyfill.js"(exports, module) {
    "use strict";
    var ctorName = require_lib35();
    function typeOf(v) {
      return ctorName(v).toLowerCase();
    }
    module.exports = typeOf;
  }
});

// node_modules/@stdlib/utils/type-of/lib/index.js
var require_lib36 = __commonJS({
  "node_modules/@stdlib/utils/type-of/lib/index.js"(exports, module) {
    "use strict";
    var usePolyfill = require_check();
    var builtin = require_main30();
    var polyfill = require_polyfill3();
    var main = usePolyfill() ? polyfill : builtin;
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-function/lib/main.js
var require_main31 = __commonJS({
  "node_modules/@stdlib/assert/is-function/lib/main.js"(exports, module) {
    "use strict";
    var typeOf = require_lib36();
    function isFunction(value) {
      return typeOf(value) === "function";
    }
    module.exports = isFunction;
  }
});

// node_modules/@stdlib/assert/is-function/lib/index.js
var require_lib37 = __commonJS({
  "node_modules/@stdlib/assert/is-function/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main31();
    module.exports = main;
  }
});

// node_modules/@stdlib/complex/float64/ctor/lib/tostring.js
var require_tostring4 = __commonJS({
  "node_modules/@stdlib/complex/float64/ctor/lib/tostring.js"(exports, module) {
    "use strict";
    function toString() {
      var str = "" + this.re;
      if (this.im < 0) {
        str += " - " + -this.im;
      } else {
        str += " + " + this.im;
      }
      str += "i";
      return str;
    }
    module.exports = toString;
  }
});

// node_modules/@stdlib/complex/float64/ctor/lib/tojson.js
var require_tojson = __commonJS({
  "node_modules/@stdlib/complex/float64/ctor/lib/tojson.js"(exports, module) {
    "use strict";
    function toJSON() {
      var out = {};
      out.type = "Complex128";
      out.re = this.re;
      out.im = this.im;
      return out;
    }
    module.exports = toJSON;
  }
});

// node_modules/@stdlib/complex/float64/ctor/lib/main.js
var require_main32 = __commonJS({
  "node_modules/@stdlib/complex/float64/ctor/lib/main.js"(exports, module) {
    "use strict";
    var isNumber = require_lib12().isPrimitive;
    var defineProperty = require_lib4();
    var setReadOnly = require_lib5();
    var format = require_lib3();
    var toStr = require_tostring4();
    var toJSON = require_tojson();
    function Complex128(real, imag) {
      if (!(this instanceof Complex128)) {
        throw new TypeError("invalid invocation. Constructor must be called with the `new` keyword.");
      }
      if (!isNumber(real)) {
        throw new TypeError(format("invalid argument. Real component must be a number. Value: `%s`.", real));
      }
      if (!isNumber(imag)) {
        throw new TypeError(format("invalid argument. Imaginary component must be a number. Value: `%s`.", imag));
      }
      defineProperty(this, "re", {
        "configurable": false,
        "enumerable": true,
        "writable": false,
        "value": real
      });
      defineProperty(this, "im", {
        "configurable": false,
        "enumerable": true,
        "writable": false,
        "value": imag
      });
      return this;
    }
    setReadOnly(Complex128, "BYTES_PER_ELEMENT", 8);
    setReadOnly(Complex128.prototype, "BYTES_PER_ELEMENT", 8);
    setReadOnly(Complex128.prototype, "byteLength", 16);
    setReadOnly(Complex128.prototype, "toString", toStr);
    setReadOnly(Complex128.prototype, "toJSON", toJSON);
    module.exports = Complex128;
  }
});

// node_modules/@stdlib/complex/float64/ctor/lib/index.js
var require_lib38 = __commonJS({
  "node_modules/@stdlib/complex/float64/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main32();
    module.exports = main;
  }
});

// node_modules/@stdlib/number/float64/base/to-float32/lib/main.js
var require_main33 = __commonJS({
  "node_modules/@stdlib/number/float64/base/to-float32/lib/main.js"(exports, module) {
    "use strict";
    var fround = typeof Math.fround === "function" ? Math.fround : null;
    module.exports = fround;
  }
});

// node_modules/@stdlib/assert/is-float32array/lib/main.js
var require_main34 = __commonJS({
  "node_modules/@stdlib/assert/is-float32array/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib10();
    var hasFloat32Array = typeof Float32Array === "function";
    function isFloat32Array(value) {
      return hasFloat32Array && value instanceof Float32Array || // eslint-disable-line stdlib/require-globals
      nativeClass(value) === "[object Float32Array]";
    }
    module.exports = isFloat32Array;
  }
});

// node_modules/@stdlib/assert/is-float32array/lib/index.js
var require_lib39 = __commonJS({
  "node_modules/@stdlib/assert/is-float32array/lib/index.js"(exports, module) {
    "use strict";
    var isFloat32Array = require_main34();
    module.exports = isFloat32Array;
  }
});

// node_modules/@stdlib/assert/has-float32array-support/lib/float32array.js
var require_float32array = __commonJS({
  "node_modules/@stdlib/assert/has-float32array-support/lib/float32array.js"(exports, module) {
    "use strict";
    var main = typeof Float32Array === "function" ? Float32Array : null;
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/has-float32array-support/lib/main.js
var require_main35 = __commonJS({
  "node_modules/@stdlib/assert/has-float32array-support/lib/main.js"(exports, module) {
    "use strict";
    var isFloat32Array = require_lib39();
    var PINF = require_lib13();
    var GlobalFloat32Array = require_float32array();
    function hasFloat32ArraySupport() {
      var bool;
      var arr;
      if (typeof GlobalFloat32Array !== "function") {
        return false;
      }
      try {
        arr = new GlobalFloat32Array([1, 3.14, -3.14, 5e40]);
        bool = isFloat32Array(arr) && arr[0] === 1 && arr[1] === 3.140000104904175 && arr[2] === -3.140000104904175 && arr[3] === PINF;
      } catch (err) {
        bool = false;
      }
      return bool;
    }
    module.exports = hasFloat32ArraySupport;
  }
});

// node_modules/@stdlib/assert/has-float32array-support/lib/index.js
var require_lib40 = __commonJS({
  "node_modules/@stdlib/assert/has-float32array-support/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat32ArraySupport = require_main35();
    module.exports = hasFloat32ArraySupport;
  }
});

// node_modules/@stdlib/array/float32/lib/main.js
var require_main36 = __commonJS({
  "node_modules/@stdlib/array/float32/lib/main.js"(exports, module) {
    "use strict";
    var ctor = typeof Float32Array === "function" ? Float32Array : void 0;
    module.exports = ctor;
  }
});

// node_modules/@stdlib/array/float32/lib/polyfill.js
var require_polyfill4 = __commonJS({
  "node_modules/@stdlib/array/float32/lib/polyfill.js"(exports, module) {
    "use strict";
    function polyfill() {
      throw new Error("not implemented");
    }
    module.exports = polyfill;
  }
});

// node_modules/@stdlib/array/float32/lib/index.js
var require_lib41 = __commonJS({
  "node_modules/@stdlib/array/float32/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat32ArraySupport = require_lib40();
    var builtin = require_main36();
    var polyfill = require_polyfill4();
    var ctor;
    if (hasFloat32ArraySupport()) {
      ctor = builtin;
    } else {
      ctor = polyfill;
    }
    module.exports = ctor;
  }
});

// node_modules/@stdlib/number/float64/base/to-float32/lib/polyfill.js
var require_polyfill5 = __commonJS({
  "node_modules/@stdlib/number/float64/base/to-float32/lib/polyfill.js"(exports, module) {
    "use strict";
    var Float32Array2 = require_lib41();
    var FLOAT32_VIEW = new Float32Array2(1);
    function float64ToFloat32(x) {
      FLOAT32_VIEW[0] = x;
      return FLOAT32_VIEW[0];
    }
    module.exports = float64ToFloat32;
  }
});

// node_modules/@stdlib/number/float64/base/to-float32/lib/index.js
var require_lib42 = __commonJS({
  "node_modules/@stdlib/number/float64/base/to-float32/lib/index.js"(exports, module) {
    "use strict";
    var builtin = require_main33();
    var polyfill = require_polyfill5();
    var float64ToFloat32;
    if (typeof builtin === "function") {
      float64ToFloat32 = builtin;
    } else {
      float64ToFloat32 = polyfill;
    }
    module.exports = float64ToFloat32;
  }
});

// node_modules/@stdlib/complex/float32/ctor/lib/tostring.js
var require_tostring5 = __commonJS({
  "node_modules/@stdlib/complex/float32/ctor/lib/tostring.js"(exports, module) {
    "use strict";
    function toString() {
      var str = "" + this.re;
      if (this.im < 0) {
        str += " - " + -this.im;
      } else {
        str += " + " + this.im;
      }
      str += "i";
      return str;
    }
    module.exports = toString;
  }
});

// node_modules/@stdlib/complex/float32/ctor/lib/tojson.js
var require_tojson2 = __commonJS({
  "node_modules/@stdlib/complex/float32/ctor/lib/tojson.js"(exports, module) {
    "use strict";
    function toJSON() {
      var out = {};
      out.type = "Complex64";
      out.re = this.re;
      out.im = this.im;
      return out;
    }
    module.exports = toJSON;
  }
});

// node_modules/@stdlib/complex/float32/ctor/lib/main.js
var require_main37 = __commonJS({
  "node_modules/@stdlib/complex/float32/ctor/lib/main.js"(exports, module) {
    "use strict";
    var isNumber = require_lib12().isPrimitive;
    var defineProperty = require_lib4();
    var setReadOnly = require_lib5();
    var float64ToFloat32 = require_lib42();
    var format = require_lib3();
    var toStr = require_tostring5();
    var toJSON = require_tojson2();
    function Complex64(real, imag) {
      if (!(this instanceof Complex64)) {
        throw new TypeError("invalid invocation. Constructor must be called with the `new` keyword.");
      }
      if (!isNumber(real)) {
        throw new TypeError(format("invalid argument. Real component must be a number. Value: `%s`.", real));
      }
      if (!isNumber(imag)) {
        throw new TypeError(format("invalid argument. Imaginary component must be a number. Value: `%s`.", imag));
      }
      defineProperty(this, "re", {
        "configurable": false,
        "enumerable": true,
        "writable": false,
        "value": float64ToFloat32(real)
      });
      defineProperty(this, "im", {
        "configurable": false,
        "enumerable": true,
        "writable": false,
        "value": float64ToFloat32(imag)
      });
      return this;
    }
    setReadOnly(Complex64, "BYTES_PER_ELEMENT", 4);
    setReadOnly(Complex64.prototype, "BYTES_PER_ELEMENT", 4);
    setReadOnly(Complex64.prototype, "byteLength", 8);
    setReadOnly(Complex64.prototype, "toString", toStr);
    setReadOnly(Complex64.prototype, "toJSON", toJSON);
    module.exports = Complex64;
  }
});

// node_modules/@stdlib/complex/float32/ctor/lib/index.js
var require_lib43 = __commonJS({
  "node_modules/@stdlib/complex/float32/ctor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main37();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-complex-like/lib/main.js
var require_main38 = __commonJS({
  "node_modules/@stdlib/assert/is-complex-like/lib/main.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var Complex64 = require_lib43();
    function isComplexLike(value) {
      if (value instanceof Complex128 || value instanceof Complex64) {
        return true;
      }
      return typeof value === "object" && value !== null && typeof value.re === "number" && typeof value.im === "number";
    }
    module.exports = isComplexLike;
  }
});

// node_modules/@stdlib/assert/is-complex-like/lib/index.js
var require_lib44 = __commonJS({
  "node_modules/@stdlib/assert/is-complex-like/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main38();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/assert/is-even/lib/main.js
var require_main39 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-even/lib/main.js"(exports, module) {
    "use strict";
    var isInteger = require_lib16();
    function isEven(x) {
      return isInteger(x / 2);
    }
    module.exports = isEven;
  }
});

// node_modules/@stdlib/math/base/assert/is-even/lib/index.js
var require_lib45 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-even/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main39();
    module.exports = main;
  }
});

// node_modules/@stdlib/array/base/assert/is-complex64array/lib/main.js
var require_main40 = __commonJS({
  "node_modules/@stdlib/array/base/assert/is-complex64array/lib/main.js"(exports, module) {
    "use strict";
    var BYTES_PER_ELEMENT = 8;
    function isComplex64Array(value) {
      return typeof value === "object" && value !== null && value.constructor.name === "Complex64Array" && value.BYTES_PER_ELEMENT === BYTES_PER_ELEMENT;
    }
    module.exports = isComplex64Array;
  }
});

// node_modules/@stdlib/array/base/assert/is-complex64array/lib/index.js
var require_lib46 = __commonJS({
  "node_modules/@stdlib/array/base/assert/is-complex64array/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main40();
    module.exports = main;
  }
});

// node_modules/@stdlib/array/base/assert/is-complex128array/lib/main.js
var require_main41 = __commonJS({
  "node_modules/@stdlib/array/base/assert/is-complex128array/lib/main.js"(exports, module) {
    "use strict";
    var BYTES_PER_ELEMENT = 16;
    function isComplex128Array(value) {
      return typeof value === "object" && value !== null && value.constructor.name === "Complex128Array" && value.BYTES_PER_ELEMENT === BYTES_PER_ELEMENT;
    }
    module.exports = isComplex128Array;
  }
});

// node_modules/@stdlib/array/base/assert/is-complex128array/lib/index.js
var require_lib47 = __commonJS({
  "node_modules/@stdlib/array/base/assert/is-complex128array/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main41();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/has-iterator-symbol-support/lib/main.js
var require_main42 = __commonJS({
  "node_modules/@stdlib/assert/has-iterator-symbol-support/lib/main.js"(exports, module) {
    "use strict";
    var hasOwnProp = require_lib8();
    var Symbol2 = require_lib9();
    function hasIteratorSymbolSupport() {
      return typeof Symbol2 === "function" && typeof Symbol2("foo") === "symbol" && hasOwnProp(Symbol2, "iterator") && typeof Symbol2.iterator === "symbol";
    }
    module.exports = hasIteratorSymbolSupport;
  }
});

// node_modules/@stdlib/assert/has-iterator-symbol-support/lib/index.js
var require_lib48 = __commonJS({
  "node_modules/@stdlib/assert/has-iterator-symbol-support/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main42();
    module.exports = main;
  }
});

// node_modules/@stdlib/symbol/iterator/lib/main.js
var require_main43 = __commonJS({
  "node_modules/@stdlib/symbol/iterator/lib/main.js"(exports, module) {
    "use strict";
    var hasIteratorSymbolSupport = require_lib48();
    var IteratorSymbol = hasIteratorSymbolSupport() ? Symbol.iterator : null;
    module.exports = IteratorSymbol;
  }
});

// node_modules/@stdlib/symbol/iterator/lib/index.js
var require_lib49 = __commonJS({
  "node_modules/@stdlib/symbol/iterator/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main43();
    module.exports = main;
  }
});

// node_modules/@stdlib/utils/define-nonenumerable-read-only-accessor/lib/main.js
var require_main44 = __commonJS({
  "node_modules/@stdlib/utils/define-nonenumerable-read-only-accessor/lib/main.js"(exports, module) {
    "use strict";
    var defineProperty = require_lib4();
    function setNonEnumerableReadOnlyAccessor(obj, prop, getter) {
      defineProperty(obj, prop, {
        "configurable": false,
        "enumerable": false,
        "get": getter
      });
    }
    module.exports = setNonEnumerableReadOnlyAccessor;
  }
});

// node_modules/@stdlib/utils/define-nonenumerable-read-only-accessor/lib/index.js
var require_lib50 = __commonJS({
  "node_modules/@stdlib/utils/define-nonenumerable-read-only-accessor/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main44();
    module.exports = main;
  }
});

// node_modules/@stdlib/assert/is-float64array/lib/main.js
var require_main45 = __commonJS({
  "node_modules/@stdlib/assert/is-float64array/lib/main.js"(exports, module) {
    "use strict";
    var nativeClass = require_lib10();
    var hasFloat64Array = typeof Float64Array === "function";
    function isFloat64Array(value) {
      return hasFloat64Array && value instanceof Float64Array || // eslint-disable-line stdlib/require-globals
      nativeClass(value) === "[object Float64Array]";
    }
    module.exports = isFloat64Array;
  }
});

// node_modules/@stdlib/assert/is-float64array/lib/index.js
var require_lib51 = __commonJS({
  "node_modules/@stdlib/assert/is-float64array/lib/index.js"(exports, module) {
    "use strict";
    var isFloat64Array = require_main45();
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
var require_main46 = __commonJS({
  "node_modules/@stdlib/assert/has-float64array-support/lib/main.js"(exports, module) {
    "use strict";
    var isFloat64Array = require_lib51();
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
var require_lib52 = __commonJS({
  "node_modules/@stdlib/assert/has-float64array-support/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat64ArraySupport = require_main46();
    module.exports = hasFloat64ArraySupport;
  }
});

// node_modules/@stdlib/array/float64/lib/main.js
var require_main47 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/main.js"(exports, module) {
    "use strict";
    var ctor = typeof Float64Array === "function" ? Float64Array : void 0;
    module.exports = ctor;
  }
});

// node_modules/@stdlib/array/float64/lib/polyfill.js
var require_polyfill6 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/polyfill.js"(exports, module) {
    "use strict";
    function polyfill() {
      throw new Error("not implemented");
    }
    module.exports = polyfill;
  }
});

// node_modules/@stdlib/array/float64/lib/index.js
var require_lib53 = __commonJS({
  "node_modules/@stdlib/array/float64/lib/index.js"(exports, module) {
    "use strict";
    var hasFloat64ArraySupport = require_lib52();
    var builtin = require_main47();
    var polyfill = require_polyfill6();
    var ctor;
    if (hasFloat64ArraySupport()) {
      ctor = builtin;
    } else {
      ctor = polyfill;
    }
    module.exports = ctor;
  }
});

// node_modules/@stdlib/complex/float64/real/lib/main.js
var require_main48 = __commonJS({
  "node_modules/@stdlib/complex/float64/real/lib/main.js"(exports, module) {
    "use strict";
    function real(z) {
      return z.re;
    }
    module.exports = real;
  }
});

// node_modules/@stdlib/complex/float64/real/lib/index.js
var require_lib54 = __commonJS({
  "node_modules/@stdlib/complex/float64/real/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main48();
    module.exports = main;
  }
});

// node_modules/@stdlib/complex/float64/imag/lib/main.js
var require_main49 = __commonJS({
  "node_modules/@stdlib/complex/float64/imag/lib/main.js"(exports, module) {
    "use strict";
    function imag(z) {
      return z.im;
    }
    module.exports = imag;
  }
});

// node_modules/@stdlib/complex/float64/imag/lib/index.js
var require_lib55 = __commonJS({
  "node_modules/@stdlib/complex/float64/imag/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main49();
    module.exports = main;
  }
});

// node_modules/@stdlib/strided/base/reinterpret-complex64/lib/main.js
var require_main50 = __commonJS({
  "node_modules/@stdlib/strided/base/reinterpret-complex64/lib/main.js"(exports, module) {
    "use strict";
    var Float32Array2 = require_lib41();
    function reinterpret2(x, offset) {
      return new Float32Array2(x.buffer, x.byteOffset + x.BYTES_PER_ELEMENT * offset, 2 * (x.length - offset));
    }
    module.exports = reinterpret2;
  }
});

// node_modules/@stdlib/strided/base/reinterpret-complex64/lib/index.js
var require_lib56 = __commonJS({
  "node_modules/@stdlib/strided/base/reinterpret-complex64/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main50();
    module.exports = main;
  }
});

// node_modules/@stdlib/strided/base/reinterpret-complex128/lib/main.js
var require_main51 = __commonJS({
  "node_modules/@stdlib/strided/base/reinterpret-complex128/lib/main.js"(exports, module) {
    "use strict";
    var Float64Array2 = require_lib53();
    function reinterpret2(x, offset) {
      return new Float64Array2(x.buffer, x.byteOffset + x.BYTES_PER_ELEMENT * offset, 2 * (x.length - offset));
    }
    module.exports = reinterpret2;
  }
});

// node_modules/@stdlib/strided/base/reinterpret-complex128/lib/index.js
var require_lib57 = __commonJS({
  "node_modules/@stdlib/strided/base/reinterpret-complex128/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main51();
    module.exports = main;
  }
});

// node_modules/@stdlib/array/base/getter/lib/main.js
var require_main52 = __commonJS({
  "node_modules/@stdlib/array/base/getter/lib/main.js"(exports, module) {
    "use strict";
    var GETTERS = {
      "float64": getFloat64,
      "float32": getFloat32,
      "int32": getInt32,
      "int16": getInt16,
      "int8": getInt8,
      "uint32": getUint32,
      "uint16": getUint16,
      "uint8": getUint8,
      "uint8c": getUint8c,
      "generic": getGeneric,
      "default": getArrayLike
    };
    function getFloat64(arr, idx) {
      return arr[idx];
    }
    function getFloat32(arr, idx) {
      return arr[idx];
    }
    function getInt32(arr, idx) {
      return arr[idx];
    }
    function getInt16(arr, idx) {
      return arr[idx];
    }
    function getInt8(arr, idx) {
      return arr[idx];
    }
    function getUint32(arr, idx) {
      return arr[idx];
    }
    function getUint16(arr, idx) {
      return arr[idx];
    }
    function getUint8(arr, idx) {
      return arr[idx];
    }
    function getUint8c(arr, idx) {
      return arr[idx];
    }
    function getGeneric(arr, idx) {
      return arr[idx];
    }
    function getArrayLike(arr, idx) {
      return arr[idx];
    }
    function getter(dtype) {
      var f = GETTERS[dtype];
      if (typeof f === "function") {
        return f;
      }
      return GETTERS.default;
    }
    module.exports = getter;
  }
});

// node_modules/@stdlib/array/base/getter/lib/index.js
var require_lib58 = __commonJS({
  "node_modules/@stdlib/array/base/getter/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main52();
    module.exports = main;
  }
});

// node_modules/@stdlib/array/base/accessor-getter/lib/main.js
var require_main53 = __commonJS({
  "node_modules/@stdlib/array/base/accessor-getter/lib/main.js"(exports, module) {
    "use strict";
    var GETTERS = {
      "complex128": getComplex128,
      "complex64": getComplex64,
      "default": getArrayLike
    };
    function getComplex128(arr, idx) {
      return arr.get(idx);
    }
    function getComplex64(arr, idx) {
      return arr.get(idx);
    }
    function getArrayLike(arr, idx) {
      return arr.get(idx);
    }
    function getter(dtype) {
      var f = GETTERS[dtype];
      if (typeof f === "function") {
        return f;
      }
      return GETTERS.default;
    }
    module.exports = getter;
  }
});

// node_modules/@stdlib/array/base/accessor-getter/lib/index.js
var require_lib59 = __commonJS({
  "node_modules/@stdlib/array/base/accessor-getter/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main53();
    module.exports = main;
  }
});

// node_modules/@stdlib/array/complex128/lib/from_iterator.js
var require_from_iterator = __commonJS({
  "node_modules/@stdlib/array/complex128/lib/from_iterator.js"(exports, module) {
    "use strict";
    var isArrayLikeObject = require_lib20();
    var isComplexLike = require_lib44();
    var format = require_lib3();
    var real = require_lib54();
    var imag = require_lib55();
    function fromIterator(it) {
      var out;
      var v;
      var z;
      out = [];
      while (true) {
        v = it.next();
        if (v.done) {
          break;
        }
        z = v.value;
        if (isArrayLikeObject(z) && z.length >= 2) {
          out.push(z[0], z[1]);
        } else if (isComplexLike(z)) {
          out.push(real(z), imag(z));
        } else {
          return new TypeError(format("invalid argument. An iterator must return either a two-element array containing real and imaginary components or a complex number. Value: `%s`.", z));
        }
      }
      return out;
    }
    module.exports = fromIterator;
  }
});

// node_modules/@stdlib/array/complex128/lib/from_iterator_map.js
var require_from_iterator_map = __commonJS({
  "node_modules/@stdlib/array/complex128/lib/from_iterator_map.js"(exports, module) {
    "use strict";
    var isArrayLikeObject = require_lib20();
    var isComplexLike = require_lib44();
    var format = require_lib3();
    var real = require_lib54();
    var imag = require_lib55();
    function fromIteratorMap(it, clbk, thisArg) {
      var out;
      var v;
      var z;
      var i;
      out = [];
      i = -1;
      while (true) {
        v = it.next();
        if (v.done) {
          break;
        }
        i += 1;
        z = clbk.call(thisArg, v.value, i);
        if (isArrayLikeObject(z) && z.length >= 2) {
          out.push(z[0], z[1]);
        } else if (isComplexLike(z)) {
          out.push(real(z), imag(z));
        } else {
          return new TypeError(format("invalid argument. Callback must return either a two-element array containing real and imaginary components or a complex number. Value: `%s`.", z));
        }
      }
      return out;
    }
    module.exports = fromIteratorMap;
  }
});

// node_modules/@stdlib/array/complex128/lib/from_array.js
var require_from_array = __commonJS({
  "node_modules/@stdlib/array/complex128/lib/from_array.js"(exports, module) {
    "use strict";
    var isComplexLike = require_lib44();
    var real = require_lib54();
    var imag = require_lib55();
    function fromArray(buf, arr) {
      var len;
      var v;
      var i;
      var j;
      len = arr.length;
      j = 0;
      for (i = 0; i < len; i++) {
        v = arr[i];
        if (!isComplexLike(v)) {
          return null;
        }
        buf[j] = real(v);
        buf[j + 1] = imag(v);
        j += 2;
      }
      return buf;
    }
    module.exports = fromArray;
  }
});

// node_modules/@stdlib/array/complex128/lib/main.js
var require_main54 = __commonJS({
  "node_modules/@stdlib/array/complex128/lib/main.js"(exports, module) {
    "use strict";
    var isNonNegativeInteger = require_lib18().isPrimitive;
    var isArrayLikeObject = require_lib20();
    var isCollection = require_lib22();
    var isArrayBuffer = require_lib23();
    var isObject = require_lib25();
    var isArray = require_lib24();
    var isStringArray = require_lib28().primitives;
    var isString = require_lib27();
    var isFunction = require_lib37();
    var isComplexLike = require_lib44();
    var isEven = require_lib45();
    var isInteger = require_lib16();
    var isComplex64Array = require_lib46();
    var isComplex128Array = require_lib47();
    var hasIteratorSymbolSupport = require_lib48();
    var ITERATOR_SYMBOL = require_lib49();
    var setReadOnly = require_lib5();
    var setReadOnlyAccessor = require_lib50();
    var Float64Array2 = require_lib53();
    var Complex128 = require_lib38();
    var real = require_lib54();
    var imag = require_lib55();
    var floor = require_lib15();
    var reinterpret64 = require_lib56();
    var reinterpret128 = require_lib57();
    var getter = require_lib58();
    var accessorGetter = require_lib59();
    var format = require_lib3();
    var fromIterator = require_from_iterator();
    var fromIteratorMap = require_from_iterator_map();
    var fromArray = require_from_array();
    var BYTES_PER_ELEMENT = Float64Array2.BYTES_PER_ELEMENT * 2;
    var HAS_ITERATOR_SYMBOL = hasIteratorSymbolSupport();
    function isComplexArray(value) {
      return value instanceof Complex128Array2 || typeof value === "object" && value !== null && (value.constructor.name === "Complex64Array" || value.constructor.name === "Complex128Array") && typeof value._length === "number" && // eslint-disable-line no-underscore-dangle
      // NOTE: we don't perform a more rigorous test here for a typed array for performance reasons, as robustly checking for a typed array instance could require walking the prototype tree and performing relatively expensive constructor checks...
      typeof value._buffer === "object";
    }
    function isComplexArrayConstructor(value) {
      return value === Complex128Array2 || // NOTE: weaker test in order to avoid a circular dependency with Complex64Array...
      value.name === "Complex64Array";
    }
    function getComplex128(buf, idx) {
      idx *= 2;
      return new Complex128(buf[idx], buf[idx + 1]);
    }
    function Complex128Array2() {
      var byteOffset;
      var nargs;
      var buf;
      var len;
      nargs = arguments.length;
      if (!(this instanceof Complex128Array2)) {
        if (nargs === 0) {
          return new Complex128Array2();
        }
        if (nargs === 1) {
          return new Complex128Array2(arguments[0]);
        }
        if (nargs === 2) {
          return new Complex128Array2(arguments[0], arguments[1]);
        }
        return new Complex128Array2(arguments[0], arguments[1], arguments[2]);
      }
      if (nargs === 0) {
        buf = new Float64Array2(0);
      } else if (nargs === 1) {
        if (isNonNegativeInteger(arguments[0])) {
          buf = new Float64Array2(arguments[0] * 2);
        } else if (isCollection(arguments[0])) {
          buf = arguments[0];
          len = buf.length;
          if (len && isArray(buf) && isComplexLike(buf[0])) {
            buf = fromArray(new Float64Array2(len * 2), buf);
            if (buf === null) {
              if (!isEven(len)) {
                throw new RangeError(format("invalid argument. Array-like object arguments must have a length which is a multiple of two. Length: `%u`.", len));
              }
              buf = new Float64Array2(arguments[0]);
            }
          } else {
            if (isComplex64Array(buf)) {
              buf = reinterpret64(buf, 0);
            } else if (isComplex128Array(buf)) {
              buf = reinterpret128(buf, 0);
            } else if (!isEven(len)) {
              throw new RangeError(format("invalid argument. Array-like object and typed array arguments must have a length which is a multiple of two. Length: `%u`.", len));
            }
            buf = new Float64Array2(buf);
          }
        } else if (isArrayBuffer(arguments[0])) {
          buf = arguments[0];
          if (!isInteger(buf.byteLength / BYTES_PER_ELEMENT)) {
            throw new RangeError(format("invalid argument. ArrayBuffer byte length must be a multiple of %u. Byte length: `%u`.", BYTES_PER_ELEMENT, buf.byteLength));
          }
          buf = new Float64Array2(buf);
        } else if (isObject(arguments[0])) {
          buf = arguments[0];
          if (HAS_ITERATOR_SYMBOL === false) {
            throw new TypeError(format("invalid argument. Environment lacks Symbol.iterator support. Must provide a length, ArrayBuffer, typed array, or array-like object. Value: `%s`.", buf));
          }
          if (!isFunction(buf[ITERATOR_SYMBOL])) {
            throw new TypeError(format("invalid argument. Must provide a length, ArrayBuffer, typed array, array-like object, or an iterable. Value: `%s`.", buf));
          }
          buf = buf[ITERATOR_SYMBOL]();
          if (!isFunction(buf.next)) {
            throw new TypeError(format("invalid argument. Must provide a length, ArrayBuffer, typed array, array-like object, or an iterable. Value: `%s`.", buf));
          }
          buf = fromIterator(buf);
          if (buf instanceof Error) {
            throw buf;
          }
          buf = new Float64Array2(buf);
        } else {
          throw new TypeError(format("invalid argument. Must provide a length, ArrayBuffer, typed array, array-like object, or an iterable. Value: `%s`.", arguments[0]));
        }
      } else {
        buf = arguments[0];
        if (!isArrayBuffer(buf)) {
          throw new TypeError(format("invalid argument. First argument must be an ArrayBuffer. Value: `%s`.", buf));
        }
        byteOffset = arguments[1];
        if (!isNonNegativeInteger(byteOffset)) {
          throw new TypeError(format("invalid argument. Byte offset must be a nonnegative integer. Value: `%s`.", byteOffset));
        }
        if (!isInteger(byteOffset / BYTES_PER_ELEMENT)) {
          throw new RangeError(format("invalid argument. Byte offset must be a multiple of %u. Value: `%u`.", BYTES_PER_ELEMENT, byteOffset));
        }
        if (nargs === 2) {
          len = buf.byteLength - byteOffset;
          if (!isInteger(len / BYTES_PER_ELEMENT)) {
            throw new RangeError(format("invalid arguments. ArrayBuffer view byte length must be a multiple of %u. View byte length: `%u`.", BYTES_PER_ELEMENT, len));
          }
          buf = new Float64Array2(buf, byteOffset);
        } else {
          len = arguments[2];
          if (!isNonNegativeInteger(len)) {
            throw new TypeError(format("invalid argument. Length must be a nonnegative integer. Value: `%s`.", len));
          }
          if (len * BYTES_PER_ELEMENT > buf.byteLength - byteOffset) {
            throw new RangeError(format("invalid arguments. ArrayBuffer has insufficient capacity. Either decrease the array length or provide a bigger buffer. Minimum capacity: `%u`.", len * BYTES_PER_ELEMENT));
          }
          buf = new Float64Array2(buf, byteOffset, len * 2);
        }
      }
      setReadOnly(this, "_buffer", buf);
      setReadOnly(this, "_length", buf.length / 2);
      return this;
    }
    setReadOnly(Complex128Array2, "BYTES_PER_ELEMENT", BYTES_PER_ELEMENT);
    setReadOnly(Complex128Array2, "name", "Complex128Array");
    setReadOnly(Complex128Array2, "from", function from(src) {
      var thisArg;
      var nargs;
      var clbk;
      var out;
      var buf;
      var tmp;
      var get;
      var len;
      var flg;
      var v;
      var i;
      var j;
      if (!isFunction(this)) {
        throw new TypeError("invalid invocation. `this` context must be a constructor.");
      }
      if (!isComplexArrayConstructor(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      nargs = arguments.length;
      if (nargs > 1) {
        clbk = arguments[1];
        if (!isFunction(clbk)) {
          throw new TypeError(format("invalid argument. Second argument must be a function. Value: `%s`.", clbk));
        }
        if (nargs > 2) {
          thisArg = arguments[2];
        }
      }
      if (isComplexArray(src)) {
        len = src.length;
        if (clbk) {
          out = new this(len);
          buf = out._buffer;
          j = 0;
          for (i = 0; i < len; i++) {
            v = clbk.call(thisArg, src.get(i), i);
            if (isComplexLike(v)) {
              buf[j] = real(v);
              buf[j + 1] = imag(v);
            } else if (isArrayLikeObject(v) && v.length >= 2) {
              buf[j] = v[0];
              buf[j + 1] = v[1];
            } else {
              throw new TypeError(format("invalid argument. Callback must return either a two-element array containing real and imaginary components or a complex number. Value: `%s`.", v));
            }
            j += 2;
          }
          return out;
        }
        return new this(src);
      }
      if (isCollection(src)) {
        if (clbk) {
          len = src.length;
          if (src.get && src.set) {
            get = accessorGetter("default");
          } else {
            get = getter("default");
          }
          for (i = 0; i < len; i++) {
            if (!isComplexLike(get(src, i))) {
              flg = true;
              break;
            }
          }
          if (flg) {
            if (!isEven(len)) {
              throw new RangeError(format("invalid argument. First argument must have a length which is a multiple of two. Length: `%u`.", len));
            }
            out = new this(len / 2);
            buf = out._buffer;
            for (i = 0; i < len; i++) {
              buf[i] = clbk.call(thisArg, get(src, i), i);
            }
            return out;
          }
          out = new this(len);
          buf = out._buffer;
          j = 0;
          for (i = 0; i < len; i++) {
            v = clbk.call(thisArg, get(src, i), i);
            if (isComplexLike(v)) {
              buf[j] = real(v);
              buf[j + 1] = imag(v);
            } else if (isArrayLikeObject(v) && v.length >= 2) {
              buf[j] = v[0];
              buf[j + 1] = v[1];
            } else {
              throw new TypeError(format("invalid argument. Callback must return either a two-element array containing real and imaginary components or a complex number. Value: `%s`.", v));
            }
            j += 2;
          }
          return out;
        }
        return new this(src);
      }
      if (isObject(src) && HAS_ITERATOR_SYMBOL && isFunction(src[ITERATOR_SYMBOL])) {
        buf = src[ITERATOR_SYMBOL]();
        if (!isFunction(buf.next)) {
          throw new TypeError(format("invalid argument. First argument must be an array-like object or an iterable. Value: `%s`.", src));
        }
        if (clbk) {
          tmp = fromIteratorMap(buf, clbk, thisArg);
        } else {
          tmp = fromIterator(buf);
        }
        if (tmp instanceof Error) {
          throw tmp;
        }
        len = tmp.length / 2;
        out = new this(len);
        buf = out._buffer;
        for (i = 0; i < len; i++) {
          buf[i] = tmp[i];
        }
        return out;
      }
      throw new TypeError(format("invalid argument. First argument must be an array-like object or an iterable. Value: `%s`.", src));
    });
    setReadOnly(Complex128Array2, "of", function of() {
      var args;
      var i;
      if (!isFunction(this)) {
        throw new TypeError("invalid invocation. `this` context must be a constructor.");
      }
      if (!isComplexArrayConstructor(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      args = [];
      for (i = 0; i < arguments.length; i++) {
        args.push(arguments[i]);
      }
      return new this(args);
    });
    setReadOnly(Complex128Array2.prototype, "at", function at(idx) {
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isInteger(idx)) {
        throw new TypeError(format("invalid argument. Must provide an integer. Value: `%s`.", idx));
      }
      if (idx < 0) {
        idx += this._length;
      }
      if (idx < 0 || idx >= this._length) {
        return;
      }
      return getComplex128(this._buffer, idx);
    });
    setReadOnlyAccessor(Complex128Array2.prototype, "buffer", function get() {
      return this._buffer.buffer;
    });
    setReadOnlyAccessor(Complex128Array2.prototype, "byteLength", function get() {
      return this._buffer.byteLength;
    });
    setReadOnlyAccessor(Complex128Array2.prototype, "byteOffset", function get() {
      return this._buffer.byteOffset;
    });
    setReadOnly(Complex128Array2.prototype, "BYTES_PER_ELEMENT", Complex128Array2.BYTES_PER_ELEMENT);
    setReadOnly(Complex128Array2.prototype, "copyWithin", function copyWithin(target, start) {
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (arguments.length === 2) {
        this._buffer.copyWithin(target * 2, start * 2);
      } else {
        this._buffer.copyWithin(target * 2, start * 2, arguments[2] * 2);
      }
      return this;
    });
    setReadOnly(Complex128Array2.prototype, "entries", function entries() {
      var buffer;
      var self2;
      var iter;
      var len;
      var FLG;
      var i;
      var j;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      self2 = this;
      buffer = this._buffer;
      len = this._length;
      i = -1;
      j = -2;
      iter = {};
      setReadOnly(iter, "next", next);
      setReadOnly(iter, "return", end);
      if (ITERATOR_SYMBOL) {
        setReadOnly(iter, ITERATOR_SYMBOL, factory);
      }
      return iter;
      function next() {
        var z;
        i += 1;
        if (FLG || i >= len) {
          return {
            "done": true
          };
        }
        j += 2;
        z = new Complex128(buffer[j], buffer[j + 1]);
        return {
          "value": [i, z],
          "done": false
        };
      }
      function end(value) {
        FLG = true;
        if (arguments.length) {
          return {
            "value": value,
            "done": true
          };
        }
        return {
          "done": true
        };
      }
      function factory() {
        return self2.entries();
      }
    });
    setReadOnly(Complex128Array2.prototype, "every", function every(predicate, thisArg) {
      var buf;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        if (!predicate.call(thisArg, getComplex128(buf, i), i, this)) {
          return false;
        }
      }
      return true;
    });
    setReadOnly(Complex128Array2.prototype, "fill", function fill(value, start, end) {
      var buf;
      var len;
      var idx;
      var re;
      var im;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isComplexLike(value)) {
        throw new TypeError(format("invalid argument. First argument must be a complex number. Value: `%s`.", value));
      }
      buf = this._buffer;
      len = this._length;
      if (arguments.length > 1) {
        if (!isInteger(start)) {
          throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", start));
        }
        if (start < 0) {
          start += len;
          if (start < 0) {
            start = 0;
          }
        }
        if (arguments.length > 2) {
          if (!isInteger(end)) {
            throw new TypeError(format("invalid argument. Third argument must be an integer. Value: `%s`.", end));
          }
          if (end < 0) {
            end += len;
            if (end < 0) {
              end = 0;
            }
          }
          if (end > len) {
            end = len;
          }
        } else {
          end = len;
        }
      } else {
        start = 0;
        end = len;
      }
      re = real(value);
      im = imag(value);
      for (i = start; i < end; i++) {
        idx = 2 * i;
        buf[idx] = re;
        buf[idx + 1] = im;
      }
      return this;
    });
    setReadOnly(Complex128Array2.prototype, "filter", function filter(predicate, thisArg) {
      var buf;
      var out;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      out = [];
      for (i = 0; i < this._length; i++) {
        z = getComplex128(buf, i);
        if (predicate.call(thisArg, z, i, this)) {
          out.push(z);
        }
      }
      return new this.constructor(out);
    });
    setReadOnly(Complex128Array2.prototype, "find", function find(predicate, thisArg) {
      var buf;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        z = getComplex128(buf, i);
        if (predicate.call(thisArg, z, i, this)) {
          return z;
        }
      }
    });
    setReadOnly(Complex128Array2.prototype, "findIndex", function findIndex(predicate, thisArg) {
      var buf;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        z = getComplex128(buf, i);
        if (predicate.call(thisArg, z, i, this)) {
          return i;
        }
      }
      return -1;
    });
    setReadOnly(Complex128Array2.prototype, "findLast", function findLast(predicate, thisArg) {
      var buf;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = this._length - 1; i >= 0; i--) {
        z = getComplex128(buf, i);
        if (predicate.call(thisArg, z, i, this)) {
          return z;
        }
      }
    });
    setReadOnly(Complex128Array2.prototype, "findLastIndex", function findLastIndex(predicate, thisArg) {
      var buf;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = this._length - 1; i >= 0; i--) {
        z = getComplex128(buf, i);
        if (predicate.call(thisArg, z, i, this)) {
          return i;
        }
      }
      return -1;
    });
    setReadOnly(Complex128Array2.prototype, "forEach", function forEach(fcn, thisArg) {
      var buf;
      var i;
      var z;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(fcn)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", fcn));
      }
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        z = getComplex128(buf, i);
        fcn.call(thisArg, z, i, this);
      }
    });
    setReadOnly(Complex128Array2.prototype, "get", function get(idx) {
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isNonNegativeInteger(idx)) {
        throw new TypeError(format("invalid argument. Must provide a nonnegative integer. Value: `%s`.", idx));
      }
      if (idx >= this._length) {
        return;
      }
      return getComplex128(this._buffer, idx);
    });
    setReadOnlyAccessor(Complex128Array2.prototype, "length", function get() {
      return this._length;
    });
    setReadOnly(Complex128Array2.prototype, "includes", function includes(searchElement, fromIndex) {
      var buf;
      var idx;
      var re;
      var im;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isComplexLike(searchElement)) {
        throw new TypeError(format("invalid argument. First argument must be a complex number. Value: `%s`.", searchElement));
      }
      if (arguments.length > 1) {
        if (!isInteger(fromIndex)) {
          throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", fromIndex));
        }
        if (fromIndex < 0) {
          fromIndex += this._length;
          if (fromIndex < 0) {
            fromIndex = 0;
          }
        }
      } else {
        fromIndex = 0;
      }
      re = real(searchElement);
      im = imag(searchElement);
      buf = this._buffer;
      for (i = fromIndex; i < this._length; i++) {
        idx = 2 * i;
        if (re === buf[idx] && im === buf[idx + 1]) {
          return true;
        }
      }
      return false;
    });
    setReadOnly(Complex128Array2.prototype, "indexOf", function indexOf(searchElement, fromIndex) {
      var buf;
      var idx;
      var re;
      var im;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isComplexLike(searchElement)) {
        throw new TypeError(format("invalid argument. First argument must be a complex number. Value: `%s`.", searchElement));
      }
      if (arguments.length > 1) {
        if (!isInteger(fromIndex)) {
          throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", fromIndex));
        }
        if (fromIndex < 0) {
          fromIndex += this._length;
          if (fromIndex < 0) {
            fromIndex = 0;
          }
        }
      } else {
        fromIndex = 0;
      }
      re = real(searchElement);
      im = imag(searchElement);
      buf = this._buffer;
      for (i = fromIndex; i < this._length; i++) {
        idx = 2 * i;
        if (re === buf[idx] && im === buf[idx + 1]) {
          return i;
        }
      }
      return -1;
    });
    setReadOnly(Complex128Array2.prototype, "join", function join(separator) {
      var out;
      var buf;
      var sep;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (arguments.length === 0) {
        sep = ",";
      } else if (isString(separator)) {
        sep = separator;
      } else {
        throw new TypeError(format("invalid argument. First argument must be a string. Value: `%s`.", separator));
      }
      out = [];
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        out.push(getComplex128(buf, i).toString());
      }
      return out.join(sep);
    });
    setReadOnly(Complex128Array2.prototype, "keys", function keys() {
      var self2;
      var iter;
      var len;
      var FLG;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      self2 = this;
      len = this._length;
      i = -1;
      iter = {};
      setReadOnly(iter, "next", next);
      setReadOnly(iter, "return", end);
      if (ITERATOR_SYMBOL) {
        setReadOnly(iter, ITERATOR_SYMBOL, factory);
      }
      return iter;
      function next() {
        i += 1;
        if (FLG || i >= len) {
          return {
            "done": true
          };
        }
        return {
          "value": i,
          "done": false
        };
      }
      function end(value) {
        FLG = true;
        if (arguments.length) {
          return {
            "value": value,
            "done": true
          };
        }
        return {
          "done": true
        };
      }
      function factory() {
        return self2.keys();
      }
    });
    setReadOnly(Complex128Array2.prototype, "lastIndexOf", function lastIndexOf(searchElement, fromIndex) {
      var buf;
      var idx;
      var re;
      var im;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isComplexLike(searchElement)) {
        throw new TypeError(format("invalid argument. First argument must be a complex number. Value: `%s`.", searchElement));
      }
      if (arguments.length > 1) {
        if (!isInteger(fromIndex)) {
          throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", fromIndex));
        }
        if (fromIndex >= this._length) {
          fromIndex = this._length - 1;
        } else if (fromIndex < 0) {
          fromIndex += this._length;
        }
      } else {
        fromIndex = this._length - 1;
      }
      re = real(searchElement);
      im = imag(searchElement);
      buf = this._buffer;
      for (i = fromIndex; i >= 0; i--) {
        idx = 2 * i;
        if (re === buf[idx] && im === buf[idx + 1]) {
          return i;
        }
      }
      return -1;
    });
    setReadOnly(Complex128Array2.prototype, "map", function map(fcn, thisArg) {
      var outbuf;
      var buf;
      var out;
      var i;
      var v;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(fcn)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", fcn));
      }
      buf = this._buffer;
      out = new this.constructor(this._length);
      outbuf = out._buffer;
      for (i = 0; i < this._length; i++) {
        v = fcn.call(thisArg, getComplex128(buf, i), i, this);
        if (isComplexLike(v)) {
          outbuf[2 * i] = real(v);
          outbuf[2 * i + 1] = imag(v);
        } else if (isArrayLikeObject(v) && v.length === 2) {
          outbuf[2 * i] = v[0];
          outbuf[2 * i + 1] = v[1];
        } else {
          throw new TypeError(format("invalid argument. Callback must return either a two-element array containing real and imaginary components or a complex number. Value: `%s`.", v));
        }
      }
      return out;
    });
    setReadOnly(Complex128Array2.prototype, "reduce", function reduce(reducer, initialValue) {
      var buf;
      var acc;
      var len;
      var v;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(reducer)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", reducer));
      }
      buf = this._buffer;
      len = this._length;
      if (arguments.length > 1) {
        acc = initialValue;
        i = 0;
      } else {
        if (len === 0) {
          throw new Error("invalid operation. If not provided an initial value, an array must contain at least one element.");
        }
        acc = getComplex128(buf, 0);
        i = 1;
      }
      for (; i < len; i++) {
        v = getComplex128(buf, i);
        acc = reducer(acc, v, i, this);
      }
      return acc;
    });
    setReadOnly(Complex128Array2.prototype, "reduceRight", function reduceRight(reducer, initialValue) {
      var buf;
      var acc;
      var len;
      var v;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(reducer)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", reducer));
      }
      buf = this._buffer;
      len = this._length;
      if (arguments.length > 1) {
        acc = initialValue;
        i = len - 1;
      } else {
        if (len === 0) {
          throw new Error("invalid operation. If not provided an initial value, an array must contain at least one element.");
        }
        acc = getComplex128(buf, len - 1);
        i = len - 2;
      }
      for (; i >= 0; i--) {
        v = getComplex128(buf, i);
        acc = reducer(acc, v, i, this);
      }
      return acc;
    });
    setReadOnly(Complex128Array2.prototype, "reverse", function reverse() {
      var buf;
      var tmp;
      var len;
      var N;
      var i;
      var j;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      len = this._length;
      buf = this._buffer;
      N = floor(len / 2);
      for (i = 0; i < N; i++) {
        j = len - i - 1;
        tmp = buf[2 * i];
        buf[2 * i] = buf[2 * j];
        buf[2 * j] = tmp;
        tmp = buf[2 * i + 1];
        buf[2 * i + 1] = buf[2 * j + 1];
        buf[2 * j + 1] = tmp;
      }
      return this;
    });
    setReadOnly(Complex128Array2.prototype, "set", function set(value) {
      var sbuf;
      var idx;
      var buf;
      var tmp;
      var flg;
      var N;
      var v;
      var i;
      var j;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      buf = this._buffer;
      if (arguments.length > 1) {
        idx = arguments[1];
        if (!isNonNegativeInteger(idx)) {
          throw new TypeError(format("invalid argument. Index argument must be a nonnegative integer. Value: `%s`.", idx));
        }
      } else {
        idx = 0;
      }
      if (isComplexLike(value)) {
        if (idx >= this._length) {
          throw new RangeError(format("invalid argument. Index argument is out-of-bounds. Value: `%u`.", idx));
        }
        idx *= 2;
        buf[idx] = real(value);
        buf[idx + 1] = imag(value);
        return;
      }
      if (isComplexArray(value)) {
        N = value._length;
        if (idx + N > this._length) {
          throw new RangeError("invalid arguments. Target array lacks sufficient storage to accommodate source values.");
        }
        sbuf = value._buffer;
        j = buf.byteOffset + idx * BYTES_PER_ELEMENT;
        if (sbuf.buffer === buf.buffer && (sbuf.byteOffset < j && sbuf.byteOffset + sbuf.byteLength > j)) {
          tmp = new Float64Array2(sbuf.length);
          for (i = 0; i < sbuf.length; i++) {
            tmp[i] = sbuf[i];
          }
          sbuf = tmp;
        }
        idx *= 2;
        j = 0;
        for (i = 0; i < N; i++) {
          buf[idx] = sbuf[j];
          buf[idx + 1] = sbuf[j + 1];
          idx += 2;
          j += 2;
        }
        return;
      }
      if (isCollection(value)) {
        N = value.length;
        for (i = 0; i < N; i++) {
          if (!isComplexLike(value[i])) {
            flg = true;
            break;
          }
        }
        if (flg) {
          if (!isEven(N)) {
            throw new RangeError(format("invalid argument. Array-like object arguments must have a length which is a multiple of two. Length: `%u`.", N));
          }
          if (idx + N / 2 > this._length) {
            throw new RangeError("invalid arguments. Target array lacks sufficient storage to accommodate source values.");
          }
          sbuf = value;
          j = buf.byteOffset + idx * BYTES_PER_ELEMENT;
          if (sbuf.buffer === buf.buffer && (sbuf.byteOffset < j && sbuf.byteOffset + sbuf.byteLength > j)) {
            tmp = new Float64Array2(N);
            for (i = 0; i < N; i++) {
              tmp[i] = sbuf[i];
            }
            sbuf = tmp;
          }
          idx *= 2;
          N /= 2;
          j = 0;
          for (i = 0; i < N; i++) {
            buf[idx] = sbuf[j];
            buf[idx + 1] = sbuf[j + 1];
            idx += 2;
            j += 2;
          }
          return;
        }
        if (idx + N > this._length) {
          throw new RangeError("invalid arguments. Target array lacks sufficient storage to accommodate source values.");
        }
        idx *= 2;
        for (i = 0; i < N; i++) {
          v = value[i];
          buf[idx] = real(v);
          buf[idx + 1] = imag(v);
          idx += 2;
        }
        return;
      }
      throw new TypeError(format("invalid argument. First argument must be either a complex number, an array-like object, or a complex number array. Value: `%s`.", value));
    });
    setReadOnly(Complex128Array2.prototype, "slice", function slice(start, end) {
      var outlen;
      var outbuf;
      var out;
      var idx;
      var buf;
      var len;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      buf = this._buffer;
      len = this._length;
      if (arguments.length === 0) {
        start = 0;
        end = len;
      } else {
        if (!isInteger(start)) {
          throw new TypeError(format("invalid argument. First argument must be an integer. Value: `%s`.", start));
        }
        if (start < 0) {
          start += len;
          if (start < 0) {
            start = 0;
          }
        }
        if (arguments.length === 1) {
          end = len;
        } else {
          if (!isInteger(end)) {
            throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", end));
          }
          if (end < 0) {
            end += len;
            if (end < 0) {
              end = 0;
            }
          } else if (end > len) {
            end = len;
          }
        }
      }
      if (start < end) {
        outlen = end - start;
      } else {
        outlen = 0;
      }
      out = new this.constructor(outlen);
      outbuf = out._buffer;
      for (i = 0; i < outlen; i++) {
        idx = 2 * (i + start);
        outbuf[2 * i] = buf[idx];
        outbuf[2 * i + 1] = buf[idx + 1];
      }
      return out;
    });
    setReadOnly(Complex128Array2.prototype, "some", function some(predicate, thisArg) {
      var buf;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(predicate)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", predicate));
      }
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        if (predicate.call(thisArg, getComplex128(buf, i), i, this)) {
          return true;
        }
      }
      return false;
    });
    setReadOnly(Complex128Array2.prototype, "sort", function sort(compareFcn) {
      var tmp;
      var buf;
      var len;
      var i;
      var j;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(compareFcn)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", compareFcn));
      }
      buf = this._buffer;
      len = this._length;
      tmp = [];
      for (i = 0; i < len; i++) {
        tmp.push(getComplex128(buf, i));
      }
      tmp.sort(compareFcn);
      for (i = 0; i < len; i++) {
        j = 2 * i;
        buf[j] = real(tmp[i]);
        buf[j + 1] = imag(tmp[i]);
      }
      return this;
    });
    setReadOnly(Complex128Array2.prototype, "subarray", function subarray(begin, end) {
      var offset;
      var buf;
      var len;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      buf = this._buffer;
      len = this._length;
      if (arguments.length === 0) {
        begin = 0;
        end = len;
      } else {
        if (!isInteger(begin)) {
          throw new TypeError(format("invalid argument. First argument must be an integer. Value: `%s`.", begin));
        }
        if (begin < 0) {
          begin += len;
          if (begin < 0) {
            begin = 0;
          }
        }
        if (arguments.length === 1) {
          end = len;
        } else {
          if (!isInteger(end)) {
            throw new TypeError(format("invalid argument. Second argument must be an integer. Value: `%s`.", end));
          }
          if (end < 0) {
            end += len;
            if (end < 0) {
              end = 0;
            }
          } else if (end > len) {
            end = len;
          }
        }
      }
      if (begin >= len) {
        len = 0;
        offset = buf.byteLength;
      } else if (begin >= end) {
        len = 0;
        offset = buf.byteOffset + begin * BYTES_PER_ELEMENT;
      } else {
        len = end - begin;
        offset = buf.byteOffset + begin * BYTES_PER_ELEMENT;
      }
      return new this.constructor(buf.buffer, offset, len < 0 ? 0 : len);
    });
    setReadOnly(Complex128Array2.prototype, "toLocaleString", function toLocaleString(locales, options) {
      var opts;
      var loc;
      var out;
      var buf;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (arguments.length === 0) {
        loc = [];
      } else if (isString(locales) || isStringArray(locales)) {
        loc = locales;
      } else {
        throw new TypeError(format("invalid argument. First argument must be a string or an array of strings. Value: `%s`.", locales));
      }
      if (arguments.length < 2) {
        opts = {};
      } else if (isObject(options)) {
        opts = options;
      } else {
        throw new TypeError(format("invalid argument. Options argument must be an object. Value: `%s`.", options));
      }
      buf = this._buffer;
      out = [];
      for (i = 0; i < this._length; i++) {
        out.push(getComplex128(buf, i).toLocaleString(loc, opts));
      }
      return out.join(",");
    });
    setReadOnly(Complex128Array2.prototype, "toReversed", function toReversed() {
      var outbuf;
      var out;
      var len;
      var buf;
      var i;
      var j;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      len = this._length;
      out = new this.constructor(len);
      buf = this._buffer;
      outbuf = out._buffer;
      for (i = 0; i < len; i++) {
        j = len - i - 1;
        outbuf[2 * i] = buf[2 * j];
        outbuf[2 * i + 1] = buf[2 * j + 1];
      }
      return out;
    });
    setReadOnly(Complex128Array2.prototype, "toSorted", function toSorted(compareFcn) {
      var tmp;
      var buf;
      var len;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isFunction(compareFcn)) {
        throw new TypeError(format("invalid argument. First argument must be a function. Value: `%s`.", compareFcn));
      }
      buf = this._buffer;
      len = this._length;
      tmp = [];
      for (i = 0; i < len; i++) {
        tmp.push(getComplex128(buf, i));
      }
      tmp.sort(compareFcn);
      return new Complex128Array2(tmp);
    });
    setReadOnly(Complex128Array2.prototype, "toString", function toString() {
      var out;
      var buf;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      out = [];
      buf = this._buffer;
      for (i = 0; i < this._length; i++) {
        out.push(getComplex128(buf, i).toString());
      }
      return out.join(",");
    });
    setReadOnly(Complex128Array2.prototype, "values", function values() {
      var iter;
      var self2;
      var len;
      var FLG;
      var buf;
      var i;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      self2 = this;
      buf = this._buffer;
      len = this._length;
      i = -1;
      iter = {};
      setReadOnly(iter, "next", next);
      setReadOnly(iter, "return", end);
      if (ITERATOR_SYMBOL) {
        setReadOnly(iter, ITERATOR_SYMBOL, factory);
      }
      return iter;
      function next() {
        i += 1;
        if (FLG || i >= len) {
          return {
            "done": true
          };
        }
        return {
          "value": getComplex128(buf, i),
          "done": false
        };
      }
      function end(value) {
        FLG = true;
        if (arguments.length) {
          return {
            "value": value,
            "done": true
          };
        }
        return {
          "done": true
        };
      }
      function factory() {
        return self2.values();
      }
    });
    setReadOnly(Complex128Array2.prototype, "with", function copyWith(index, value) {
      var buf;
      var out;
      var len;
      if (!isComplexArray(this)) {
        throw new TypeError("invalid invocation. `this` is not a complex number array.");
      }
      if (!isInteger(index)) {
        throw new TypeError(format("invalid argument. First argument must be an integer. Value: `%s`.", index));
      }
      len = this._length;
      if (index < 0) {
        index += len;
      }
      if (index < 0 || index >= len) {
        throw new RangeError(format("invalid argument. Index argument is out-of-bounds. Value: `%s`.", index));
      }
      if (!isComplexLike(value)) {
        throw new TypeError(format("invalid argument. Second argument must be a complex number. Value: `%s`.", value));
      }
      out = new this.constructor(this._buffer);
      buf = out._buffer;
      buf[2 * index] = real(value);
      buf[2 * index + 1] = imag(value);
      return out;
    });
    module.exports = Complex128Array2;
  }
});

// node_modules/@stdlib/array/complex128/lib/index.js
var require_lib60 = __commonJS({
  "node_modules/@stdlib/array/complex128/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main54();
    module.exports = main;
  }
});

// lib/lapack/base/dlamch/lib/base.js
var require_base = __commonJS({
  "lib/lapack/base/dlamch/lib/base.js"(exports, module) {
    "use strict";
    var EPS = 11102230246251565e-32;
    var SFMIN = 22250738585072014e-324;
    var BASE2 = 2;
    var PREC = EPS * BASE2;
    var DIGITS = 53;
    var RND = 1;
    var EMIN = -1021;
    var RMIN = 22250738585072014e-324;
    var EMAX = 1024;
    var RMAX = 17976931348623157e292;
    function dlamch(cmach) {
      var c = cmach.charAt(0).toUpperCase();
      if (c === "E") {
        return EPS;
      }
      if (c === "S") {
        return SFMIN;
      }
      if (c === "B") {
        return BASE2;
      }
      if (c === "P") {
        return PREC;
      }
      if (c === "N") {
        return DIGITS;
      }
      if (c === "R") {
        return RND;
      }
      if (c === "M") {
        return EMIN;
      }
      if (c === "U") {
        return RMIN;
      }
      if (c === "L") {
        return EMAX;
      }
      if (c === "O") {
        return RMAX;
      }
      return 0;
    }
    module.exports = dlamch;
  }
});

// lib/lapack/base/dlascl/lib/base.js
var require_base2 = __commonJS({
  "lib/lapack/base/dlascl/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base();
    function dlascl(type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA) {
      var smlnum;
      var bignum;
      var cfromc;
      var cfrom1;
      var ctoc;
      var cto1;
      var done;
      var itype;
      var iMax;
      var iMin;
      var mul;
      var k1;
      var k2;
      var k3;
      var k4;
      var ai;
      var c;
      var i;
      var j;
      c = type.charAt(0).toUpperCase();
      if (c === "G") {
        itype = 0;
      } else if (c === "L") {
        itype = 1;
      } else if (c === "U") {
        itype = 2;
      } else if (c === "H") {
        itype = 3;
      } else if (c === "B") {
        itype = 4;
      } else if (c === "Q") {
        itype = 5;
      } else if (c === "Z") {
        itype = 6;
      } else {
        return -1;
      }
      if (N === 0 || M === 0) {
        return 0;
      }
      smlnum = dlamch("S");
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

// lib/lapack/base/dlartg/lib/base.js
var require_base3 = __commonJS({
  "lib/lapack/base/dlartg/lib/base.js"(exports, module) {
    "use strict";
    var SAFMIN2 = 22250738585072014e-324;
    var SAFMAX2 = 449423283715579e293;
    function dlartg(f, g) {
      var rtmin;
      var rtmax;
      var f1;
      var g1;
      var fs;
      var gs;
      var d;
      var c;
      var s;
      var r;
      var u;
      rtmin = Math.sqrt(SAFMIN2);
      rtmax = Math.sqrt(SAFMAX2 / 2);
      f1 = Math.abs(f);
      g1 = Math.abs(g);
      if (g === 0) {
        c = 1;
        s = 0;
        r = f;
      } else if (f === 0) {
        c = 0;
        s = g > 0 ? 1 : -1;
        r = g1;
      } else if (f1 > rtmin && f1 < rtmax && g1 > rtmin && g1 < rtmax) {
        d = Math.sqrt(f * f + g * g);
        c = f1 / d;
        r = f > 0 ? d : -d;
        s = g / r;
      } else {
        u = Math.min(SAFMAX2, Math.max(SAFMIN2, f1, g1));
        fs = f / u;
        gs = g / u;
        d = Math.sqrt(fs * fs + gs * gs);
        c = Math.abs(fs) / d;
        r = f > 0 ? d : -d;
        s = gs / r;
        r = r * u;
      }
      return {
        "c": c,
        "s": s,
        "r": r
      };
    }
    module.exports = dlartg;
  }
});

// lib/lapack/base/dlas2/lib/base.js
var require_base4 = __commonJS({
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
      } else {
        if (ga < fhmx) {
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
            out[0] = out[0] + out[0];
            out[1] = ga / (c + c);
          }
        }
      }
      return out;
    }
    module.exports = dlas2;
  }
});

// lib/blas/base/dcopy/lib/base.js
var require_base5 = __commonJS({
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

// lib/lapack/base/disnan/lib/base.js
var require_base6 = __commonJS({
  "lib/lapack/base/disnan/lib/base.js"(exports, module) {
    "use strict";
    function disnan(din) {
      return din !== din;
    }
    module.exports = disnan;
  }
});

// lib/lapack/base/dlasq4/lib/base.js
var require_base7 = __commonJS({
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
        return { "tau": tau, "ttype": ttype, "g": g };
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
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b2 = Z(nn - 5) / Z(nn - 7);
              np = nn - 9;
            } else {
              np = nn - 2 * pp;
              gam = dn1;
              if (Z(np - 4) > Z(np - 2)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              a2 = Z(np - 4) / Z(np - 2);
              if (Z(nn - 9) > Z(nn - 11)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b2 = Z(nn - 9) / Z(nn - 11);
              np = nn - 13;
            }
            a2 = a2 + b2;
            for (i4 = np; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (b2 === 0) {
                break;
              }
              b1 = b2;
              if (Z(i4) > Z(i4 - 2)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b2 = b2 * (Z(i4) / Z(i4 - 2));
              a2 = a2 + b2;
              if (HUNDRD * Math.max(b2, b1) < a2 || CNST1 < a2) {
                break;
              }
            }
            a2 = CNST3 * a2;
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
            return { "tau": tau0, "ttype": ttype, "g": g };
          }
          a2 = Z(np - 8) / b2 * (1 + Z(np - 4) / b1);
          if (n0 - i0 > 2) {
            b2 = Z(nn - 13) / Z(nn - 15);
            a2 = a2 + b2;
            for (i4 = nn - 17; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (b2 === 0) {
                break;
              }
              b1 = b2;
              if (Z(i4) > Z(i4 - 2)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b2 = b2 * (Z(i4) / Z(i4 - 2));
              a2 = a2 + b2;
              if (HUNDRD * Math.max(b2, b1) < a2 || CNST1 < a2) {
                break;
              }
            }
            a2 = CNST3 * a2;
          }
          if (a2 < CNST1) {
            s = gam * (1 - Math.sqrt(a2)) / (1 + a2);
          }
        } else {
          if (ttype === -6) {
            g = g + THIRD * (1 - g);
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
            return { "tau": tau0, "ttype": ttype, "g": g };
          }
          b1 = Z(nn - 5) / Z(nn - 7);
          b2 = b1;
          if (b2 !== 0) {
            for (i4 = 4 * n0 - 9 + pp; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              a2 = b1;
              if (Z(i4) > Z(i4 - 2)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b1 = b1 * (Z(i4) / Z(i4 - 2));
              b2 = b2 + b1;
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
            return { "tau": tau0, "ttype": ttype, "g": g };
          }
          b1 = Z(nn - 5) / Z(nn - 7);
          b2 = b1;
          if (b2 !== 0) {
            for (i4 = 4 * n0 - 9 + pp; i4 >= 4 * i0 - 1 + pp; i4 -= 4) {
              if (Z(i4) > Z(i4 - 2)) {
                return { "tau": tau0, "ttype": ttype, "g": g };
              }
              b1 = b1 * (Z(i4) / Z(i4 - 2));
              b2 = b2 + b1;
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
      return { "tau": tau, "ttype": ttype, "g": g };
    }
    module.exports = dlasq4;
  }
});

// lib/lapack/base/dlasq5/lib/base.js
var require_base8 = __commonJS({
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
      if (tau !== 0) {
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
var require_base9 = __commonJS({
  "lib/lapack/base/dlasq6/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base();
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
            d = d * temp;
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
            d = d * temp;
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
var require_base10 = __commonJS({
  "lib/lapack/base/dlasq3/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base();
    var disnan = require_base6();
    var dlasq4 = require_base7();
    var dlasq5 = require_base8();
    var dlasq6 = require_base9();
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
        } else {
          if (Z(nn - 5) > tol2 * (sigma + Z(nn - 3)) && Z(nn - 2 * pp - 4) > tol2 * Z(nn - 7)) {
            if (Z(nn - 9) > tol2 * sigma && Z(nn - 2 * pp - 8) > tol2 * Z(nn - 11)) {
              break;
            }
          } else {
            setZ(4 * n0 - 3, Z(4 * n0 + pp - 3) + sigma);
            n0 -= 1;
            continue;
          }
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
            tau = QURTR * tau;
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
var require_base11 = __commonJS({
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
      if (id === "D" || id === "d") {
        dir = 0;
      } else if (id === "I" || id === "i") {
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
      stack = new Array(64);
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
          } else {
            if (d3 < d2) {
              dmnmx = d2;
            } else if (d3 < d1) {
              dmnmx = d3;
            } else {
              dmnmx = d1;
            }
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
var require_base12 = __commonJS({
  "lib/lapack/base/dlasq2/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base();
    var dlasq3 = require_base10();
    var dlasrt = require_base11();
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
        dlasrt("D", N, z, stride, offset);
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
            d = d * temp;
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
          dlasrt("D", N, z, stride, offset);
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
var require_base13 = __commonJS({
  "lib/lapack/base/dlasq1/lib/base.js"(exports, module) {
    "use strict";
    var dcopy = require_base5();
    var dlamch = require_base();
    var dlas2 = require_base4();
    var dlascl = require_base2();
    var dlasq2 = require_base12();
    var dlasrt = require_base11();
    var ZERO = 0;
    function dlasq1(N, d, strideD, offsetD, e, strideE, offsetE, WORK, strideWORK, offsetWORK) {
      var sigmx;
      var sigmn;
      var scale;
      var safmin;
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
        out = new Float64Array(2);
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
        dlasrt("D", N, d, strideD, offsetD);
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
      dlascl("G", 0, 0, sigmx, scale, 2 * N - 1, 1, WORK, strideWORK, (2 * N - 1) * strideWORK, offsetWORK);
      iw = offsetWORK;
      for (i = 0; i < 2 * N - 1; i++) {
        WORK[iw] = WORK[iw] * WORK[iw];
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
        dlascl("G", 0, 0, scale, sigmx, N, 1, d, strideD, N * strideD, offsetD);
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
        dlascl("G", 0, 0, scale, sigmx, N, 1, d, strideD, N * strideD, offsetD);
        dlascl("G", 0, 0, scale, sigmx, N, 1, e, strideE, N * strideE, offsetE);
      }
      return info;
    }
    module.exports = dlasq1;
  }
});

// lib/lapack/base/dlasv2/lib/base.js
var require_base14 = __commonJS({
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

// lib/blas/base/zdrot/lib/base.js
var require_base15 = __commonJS({
  "lib/blas/base/zdrot/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zdrot(N, zx, strideX, offsetX, zy, strideY, offsetY, c, s) {
      var tempr;
      var tempi;
      var xv;
      var yv;
      var sx;
      var sy;
      var ix;
      var iy;
      var i;
      if (N <= 0) {
        return zx;
      }
      xv = reinterpret2(zx, 0);
      yv = reinterpret2(zy, 0);
      ix = offsetX * 2;
      iy = offsetY * 2;
      sx = strideX * 2;
      sy = strideY * 2;
      for (i = 0; i < N; i++) {
        tempr = c * xv[ix] + s * yv[iy];
        tempi = c * xv[ix + 1] + s * yv[iy + 1];
        yv[iy] = c * yv[iy] - s * xv[ix];
        yv[iy + 1] = c * yv[iy + 1] - s * xv[ix + 1];
        xv[ix] = tempr;
        xv[ix + 1] = tempi;
        ix += sx;
        iy += sy;
      }
      return zx;
    }
    module.exports = zdrot;
  }
});

// lib/blas/base/zdscal/lib/base.js
var require_base16 = __commonJS({
  "lib/blas/base/zdscal/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zdscal(N, da, zx, strideX, offsetX) {
      var xv;
      var sx;
      var ix;
      var i;
      if (N <= 0) {
        return zx;
      }
      xv = reinterpret2(zx, 0);
      ix = offsetX * 2;
      sx = strideX * 2;
      for (i = 0; i < N; i++) {
        xv[ix] *= da;
        xv[ix + 1] *= da;
        ix += sx;
      }
      return zx;
    }
    module.exports = zdscal;
  }
});

// lib/lapack/base/zlasr/lib/base.js
var require_base17 = __commonJS({
  "lib/lapack/base/zlasr/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zlasr(side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA) {
      var ctemp;
      var stemp;
      var tempRe;
      var tempIm;
      var aRe;
      var aIm;
      var Av;
      var sa1;
      var sa2;
      var oA;
      var idx1;
      var idx2;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return A;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      if (side === "L" || side === "l") {
        if (pivot === "V" || pivot === "v") {
          if (direct === "F" || direct === "f") {
            for (j = 0; j < M - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + (j + 1) * sa1 + i * sa2;
                  idx2 = oA + j * sa1 + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          } else {
            for (j = M - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + (j + 1) * sa1 + i * sa2;
                  idx2 = oA + j * sa1 + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          }
        } else if (pivot === "T" || pivot === "t") {
          if (direct === "F" || direct === "f") {
            for (j = 1; j < M; j++) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + j * sa1 + i * sa2;
                  idx2 = oA + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          } else {
            for (j = M - 1; j >= 1; j--) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + j * sa1 + i * sa2;
                  idx2 = oA + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          }
        } else if (pivot === "B" || pivot === "b") {
          if (direct === "F" || direct === "f") {
            for (j = 0; j < M - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + j * sa1 + i * sa2;
                  idx2 = oA + (M - 1) * sa1 + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = stemp * aRe + ctemp * tempRe;
                  Av[idx1 + 1] = stemp * aIm + ctemp * tempIm;
                  Av[idx2] = ctemp * aRe - stemp * tempRe;
                  Av[idx2 + 1] = ctemp * aIm - stemp * tempIm;
                }
              }
            }
          } else {
            for (j = M - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < N; i++) {
                  idx1 = oA + j * sa1 + i * sa2;
                  idx2 = oA + (M - 1) * sa1 + i * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = stemp * aRe + ctemp * tempRe;
                  Av[idx1 + 1] = stemp * aIm + ctemp * tempIm;
                  Av[idx2] = ctemp * aRe - stemp * tempRe;
                  Av[idx2 + 1] = ctemp * aIm - stemp * tempIm;
                }
              }
            }
          }
        }
      } else if (side === "R" || side === "r") {
        if (pivot === "V" || pivot === "v") {
          if (direct === "F" || direct === "f") {
            for (j = 0; j < N - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + (j + 1) * sa2;
                  idx2 = oA + i * sa1 + j * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          } else {
            for (j = N - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + (j + 1) * sa2;
                  idx2 = oA + i * sa1 + j * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          }
        } else if (pivot === "T" || pivot === "t") {
          if (direct === "F" || direct === "f") {
            for (j = 1; j < N; j++) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + j * sa2;
                  idx2 = oA + i * sa1;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          } else {
            for (j = N - 1; j >= 1; j--) {
              ctemp = c[offsetC + (j - 1) * strideC];
              stemp = s[offsetS + (j - 1) * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + j * sa2;
                  idx2 = oA + i * sa1;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = ctemp * tempRe - stemp * aRe;
                  Av[idx1 + 1] = ctemp * tempIm - stemp * aIm;
                  Av[idx2] = stemp * tempRe + ctemp * aRe;
                  Av[idx2 + 1] = stemp * tempIm + ctemp * aIm;
                }
              }
            }
          }
        } else if (pivot === "B" || pivot === "b") {
          if (direct === "F" || direct === "f") {
            for (j = 0; j < N - 1; j++) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + j * sa2;
                  idx2 = oA + i * sa1 + (N - 1) * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = stemp * aRe + ctemp * tempRe;
                  Av[idx1 + 1] = stemp * aIm + ctemp * tempIm;
                  Av[idx2] = ctemp * aRe - stemp * tempRe;
                  Av[idx2 + 1] = ctemp * aIm - stemp * tempIm;
                }
              }
            }
          } else {
            for (j = N - 2; j >= 0; j--) {
              ctemp = c[offsetC + j * strideC];
              stemp = s[offsetS + j * strideS];
              if (ctemp !== 1 || stemp !== 0) {
                for (i = 0; i < M; i++) {
                  idx1 = oA + i * sa1 + j * sa2;
                  idx2 = oA + i * sa1 + (N - 1) * sa2;
                  tempRe = Av[idx1];
                  tempIm = Av[idx1 + 1];
                  aRe = Av[idx2];
                  aIm = Av[idx2 + 1];
                  Av[idx1] = stemp * aRe + ctemp * tempRe;
                  Av[idx1 + 1] = stemp * aIm + ctemp * tempIm;
                  Av[idx2] = ctemp * aRe - stemp * tempRe;
                  Av[idx2 + 1] = ctemp * aIm - stemp * tempIm;
                }
              }
            }
          }
        }
      }
      return A;
    }
    module.exports = zlasr;
  }
});

// lib/blas/base/zswap/lib/base.js
var require_base18 = __commonJS({
  "lib/blas/base/zswap/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zswap(N, zx, strideX, offsetX, zy, strideY, offsetY) {
      var tmp0;
      var tmp1;
      var xv;
      var yv;
      var sx;
      var sy;
      var ix;
      var iy;
      var i;
      if (N <= 0) {
        return zx;
      }
      xv = reinterpret2(zx, 0);
      yv = reinterpret2(zy, 0);
      ix = offsetX * 2;
      iy = offsetY * 2;
      sx = strideX * 2;
      sy = strideY * 2;
      for (i = 0; i < N; i++) {
        tmp0 = xv[ix];
        tmp1 = xv[ix + 1];
        xv[ix] = yv[iy];
        xv[ix + 1] = yv[iy + 1];
        yv[iy] = tmp0;
        yv[iy + 1] = tmp1;
        ix += sx;
        iy += sy;
      }
      return zx;
    }
    module.exports = zswap;
  }
});

// lib/lapack/base/zbdsqr/lib/base.js
var require_base19 = __commonJS({
  "lib/lapack/base/zbdsqr/lib/base.js"(exports, module) {
    "use strict";
    var dlamch = require_base();
    var dlartg = require_base3();
    var dlas2 = require_base4();
    var dlasq1 = require_base13();
    var dlasv2 = require_base14();
    var zdrot = require_base15();
    var zdscal = require_base16();
    var zlasr = require_base17();
    var zswap = require_base18();
    var ZERO = 0;
    var ONE = 1;
    var NEGONE = -1;
    var HNDRTH = 0.01;
    var TEN = 10;
    var HNDRD = 100;
    var MEIGTH = -0.125;
    var MAXITR = 6;
    var DOUT = new Float64Array(2);
    function sign(a, b) {
      var mag = Math.abs(a);
      if (b > 0 || b === 0 && !Object.is(b, -0)) {
        return mag;
      }
      return -mag;
    }
    function zbdsqr(uplo, N, ncvt, nru, ncc, d, strideD, offsetD, e, strideE, offsetE, VT, strideVT1, strideVT2, offsetVT, U, strideU1, strideU2, offsetU, C, strideC1, strideC2, offsetC, RWORK, strideRWORK, offsetRWORK) {
      var maxitdivn;
      var iterdivn;
      var rotate;
      var thresh;
      var sminoa;
      var lower;
      var oldcs;
      var oldsn;
      var shift;
      var sigmn;
      var sigmx;
      var tolmul;
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
      var isub;
      var iter;
      var dout;
      var svd2;
      var rot;
      var eps;
      var nm1;
      var nm12;
      var nm13;
      var tol;
      var sll;
      var cs;
      var sn;
      var mu;
      var ll;
      var lll;
      var m;
      var f;
      var g;
      var h;
      var r;
      var i;
      var j;
      dout = DOUT;
      info = 0;
      if (N === 0) {
        return 0;
      }
      lower = uplo === "L" || uplo === "l";
      if (N === 1) {
        if (d[offsetD] < ZERO) {
          d[offsetD] = -d[offsetD];
          if (ncvt > 0) {
            zdscal(ncvt, NEGONE, VT, strideVT2, offsetVT);
          }
        }
        return 0;
      }
      rotate = ncvt > 0 || nru > 0 || ncc > 0;
      if (!rotate) {
        info = dlasq1(N, d, strideD, offsetD, e, strideE, offsetE, RWORK, strideRWORK, offsetRWORK);
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
          rot = dlartg(d[offsetD + i * strideD], e[offsetE + i * strideE]);
          cs = rot.c;
          sn = rot.s;
          r = rot.r;
          d[offsetD + i * strideD] = r;
          e[offsetE + i * strideE] = sn * d[offsetD + (i + 1) * strideD];
          d[offsetD + (i + 1) * strideD] = cs * d[offsetD + (i + 1) * strideD];
          RWORK[offsetRWORK + i * strideRWORK] = cs;
          RWORK[offsetRWORK + (nm1 + i) * strideRWORK] = sn;
        }
        if (nru > 0) {
          zlasr(
            "R",
            "V",
            "F",
            nru,
            N,
            RWORK,
            strideRWORK,
            offsetRWORK,
            RWORK,
            strideRWORK,
            offsetRWORK + nm1 * strideRWORK,
            U,
            strideU1,
            strideU2,
            offsetU
          );
        }
        if (ncc > 0) {
          zlasr(
            "L",
            "V",
            "F",
            N,
            ncc,
            RWORK,
            strideRWORK,
            offsetRWORK,
            RWORK,
            strideRWORK,
            offsetRWORK + nm1 * strideRWORK,
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
        sminoa = sminoa / Math.sqrt(N);
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
      outer:
        while (true) {
          if (m <= 0) {
            break;
          }
          if (iter >= N) {
            iter = iter - N;
            iterdivn = iterdivn + 1;
            if (iterdivn >= maxitdivn) {
              info = 0;
              for (i = 0; i < N - 1; i++) {
                if (e[offsetE + i * strideE] !== ZERO) {
                  info = info + 1;
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
                m = m - 1;
                continue outer;
              }
              ll = ll + 1;
              break;
            }
            smax = Math.max(smax, abss, abse);
            if (lll === m - 1) {
              ll = 0;
              break;
            }
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
              zdrot(
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
              zdrot(
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
              zdrot(
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
            m = m - 2;
            continue;
          }
          if (ll > oldm || m < oldll) {
            if (Math.abs(d[offsetD + ll * strideD]) >= Math.abs(d[offsetD + m * strideD])) {
              idir = 1;
            } else {
              idir = 2;
            }
          }
          if (idir === 1) {
            if (Math.abs(e[offsetE + (m - 1) * strideE]) <= Math.abs(tol) * Math.abs(d[offsetD + m * strideD]) || tol < ZERO && Math.abs(e[offsetE + (m - 1) * strideE]) <= thresh) {
              e[offsetE + (m - 1) * strideE] = ZERO;
              continue;
            }
            if (tol >= ZERO) {
              mu = Math.abs(d[offsetD + ll * strideD]);
              smin = mu;
              for (lll = ll; lll < m; lll++) {
                if (Math.abs(e[offsetE + lll * strideE]) <= tol * mu) {
                  e[offsetE + lll * strideE] = ZERO;
                  continue outer;
                }
                mu = Math.abs(d[offsetD + (lll + 1) * strideD]) * (mu / (mu + Math.abs(e[offsetE + lll * strideE])));
                smin = Math.min(smin, mu);
              }
            }
          } else {
            if (Math.abs(e[offsetE + ll * strideE]) <= Math.abs(tol) * Math.abs(d[offsetD + ll * strideD]) || tol < ZERO && Math.abs(e[offsetE + ll * strideE]) <= thresh) {
              e[offsetE + ll * strideE] = ZERO;
              continue;
            }
            if (tol >= ZERO) {
              mu = Math.abs(d[offsetD + m * strideD]);
              smin = mu;
              for (lll = m - 1; lll >= ll; lll--) {
                if (Math.abs(e[offsetE + lll * strideE]) <= tol * mu) {
                  e[offsetE + lll * strideE] = ZERO;
                  continue outer;
                }
                mu = Math.abs(d[offsetD + lll * strideD]) * (mu / (mu + Math.abs(e[offsetE + lll * strideE])));
                smin = Math.min(smin, mu);
              }
            }
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
                rot = dlartg(d[offsetD + i * strideD] * cs, e[offsetE + i * strideE]);
                cs = rot.c;
                sn = rot.s;
                r = rot.r;
                if (i > ll) {
                  e[offsetE + (i - 1) * strideE] = oldsn * r;
                }
                rot = dlartg(oldcs * r, d[offsetD + (i + 1) * strideD] * sn);
                oldcs = rot.c;
                oldsn = rot.s;
                d[offsetD + i * strideD] = rot.r;
                RWORK[offsetRWORK + (i - ll) * strideRWORK] = cs;
                RWORK[offsetRWORK + (i - ll + nm1) * strideRWORK] = sn;
                RWORK[offsetRWORK + (i - ll + nm12) * strideRWORK] = oldcs;
                RWORK[offsetRWORK + (i - ll + nm13) * strideRWORK] = oldsn;
              }
              h = d[offsetD + m * strideD] * cs;
              d[offsetD + m * strideD] = h * oldcs;
              e[offsetE + (m - 1) * strideE] = h * oldsn;
              if (ncvt > 0) {
                zlasr(
                  "L",
                  "V",
                  "F",
                  m - ll + 1,
                  ncvt,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
                  VT,
                  strideVT1,
                  strideVT2,
                  offsetVT + ll * strideVT1
                );
              }
              if (nru > 0) {
                zlasr(
                  "R",
                  "V",
                  "F",
                  nru,
                  m - ll + 1,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
                  U,
                  strideU1,
                  strideU2,
                  offsetU + ll * strideU2
                );
              }
              if (ncc > 0) {
                zlasr(
                  "L",
                  "V",
                  "F",
                  m - ll + 1,
                  ncc,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
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
                rot = dlartg(d[offsetD + i * strideD] * cs, e[offsetE + (i - 1) * strideE]);
                cs = rot.c;
                sn = rot.s;
                r = rot.r;
                if (i < m) {
                  e[offsetE + i * strideE] = oldsn * r;
                }
                rot = dlartg(oldcs * r, d[offsetD + (i - 1) * strideD] * sn);
                oldcs = rot.c;
                oldsn = rot.s;
                d[offsetD + i * strideD] = rot.r;
                RWORK[offsetRWORK + (i - ll - 1) * strideRWORK] = cs;
                RWORK[offsetRWORK + (i - ll - 1 + nm1) * strideRWORK] = -sn;
                RWORK[offsetRWORK + (i - ll - 1 + nm12) * strideRWORK] = oldcs;
                RWORK[offsetRWORK + (i - ll - 1 + nm13) * strideRWORK] = -oldsn;
              }
              h = d[offsetD + ll * strideD] * cs;
              d[offsetD + ll * strideD] = h * oldcs;
              e[offsetE + ll * strideE] = h * oldsn;
              if (ncvt > 0) {
                zlasr(
                  "L",
                  "V",
                  "B",
                  m - ll + 1,
                  ncvt,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
                  VT,
                  strideVT1,
                  strideVT2,
                  offsetVT + ll * strideVT1
                );
              }
              if (nru > 0) {
                zlasr(
                  "R",
                  "V",
                  "B",
                  nru,
                  m - ll + 1,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
                  U,
                  strideU1,
                  strideU2,
                  offsetU + ll * strideU2
                );
              }
              if (ncc > 0) {
                zlasr(
                  "L",
                  "V",
                  "B",
                  m - ll + 1,
                  ncc,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
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
          } else {
            if (idir === 1) {
              f = (Math.abs(d[offsetD + ll * strideD]) - shift) * (sign(ONE, d[offsetD + ll * strideD]) + shift / d[offsetD + ll * strideD]);
              g = e[offsetE + ll * strideE];
              for (i = ll; i < m; i++) {
                rot = dlartg(f, g);
                cosr = rot.c;
                sinr = rot.s;
                r = rot.r;
                if (i > ll) {
                  e[offsetE + (i - 1) * strideE] = r;
                }
                f = cosr * d[offsetD + i * strideD] + sinr * e[offsetE + i * strideE];
                e[offsetE + i * strideE] = cosr * e[offsetE + i * strideE] - sinr * d[offsetD + i * strideD];
                g = sinr * d[offsetD + (i + 1) * strideD];
                d[offsetD + (i + 1) * strideD] = cosr * d[offsetD + (i + 1) * strideD];
                rot = dlartg(f, g);
                cosl = rot.c;
                sinl = rot.s;
                d[offsetD + i * strideD] = rot.r;
                f = cosl * e[offsetE + i * strideE] + sinl * d[offsetD + (i + 1) * strideD];
                d[offsetD + (i + 1) * strideD] = cosl * d[offsetD + (i + 1) * strideD] - sinl * e[offsetE + i * strideE];
                if (i < m - 1) {
                  g = sinl * e[offsetE + (i + 1) * strideE];
                  e[offsetE + (i + 1) * strideE] = cosl * e[offsetE + (i + 1) * strideE];
                }
                RWORK[offsetRWORK + (i - ll) * strideRWORK] = cosr;
                RWORK[offsetRWORK + (i - ll + nm1) * strideRWORK] = sinr;
                RWORK[offsetRWORK + (i - ll + nm12) * strideRWORK] = cosl;
                RWORK[offsetRWORK + (i - ll + nm13) * strideRWORK] = sinl;
              }
              e[offsetE + (m - 1) * strideE] = f;
              if (ncvt > 0) {
                zlasr(
                  "L",
                  "V",
                  "F",
                  m - ll + 1,
                  ncvt,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
                  VT,
                  strideVT1,
                  strideVT2,
                  offsetVT + ll * strideVT1
                );
              }
              if (nru > 0) {
                zlasr(
                  "R",
                  "V",
                  "F",
                  nru,
                  m - ll + 1,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
                  U,
                  strideU1,
                  strideU2,
                  offsetU + ll * strideU2
                );
              }
              if (ncc > 0) {
                zlasr(
                  "L",
                  "V",
                  "F",
                  m - ll + 1,
                  ncc,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
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
                rot = dlartg(f, g);
                cosr = rot.c;
                sinr = rot.s;
                r = rot.r;
                if (i < m) {
                  e[offsetE + i * strideE] = r;
                }
                f = cosr * d[offsetD + i * strideD] + sinr * e[offsetE + (i - 1) * strideE];
                e[offsetE + (i - 1) * strideE] = cosr * e[offsetE + (i - 1) * strideE] - sinr * d[offsetD + i * strideD];
                g = sinr * d[offsetD + (i - 1) * strideD];
                d[offsetD + (i - 1) * strideD] = cosr * d[offsetD + (i - 1) * strideD];
                rot = dlartg(f, g);
                cosl = rot.c;
                sinl = rot.s;
                d[offsetD + i * strideD] = rot.r;
                f = cosl * e[offsetE + (i - 1) * strideE] + sinl * d[offsetD + (i - 1) * strideD];
                d[offsetD + (i - 1) * strideD] = cosl * d[offsetD + (i - 1) * strideD] - sinl * e[offsetE + (i - 1) * strideE];
                if (i > ll + 1) {
                  g = sinl * e[offsetE + (i - 2) * strideE];
                  e[offsetE + (i - 2) * strideE] = cosl * e[offsetE + (i - 2) * strideE];
                }
                RWORK[offsetRWORK + (i - ll - 1) * strideRWORK] = cosr;
                RWORK[offsetRWORK + (i - ll - 1 + nm1) * strideRWORK] = -sinr;
                RWORK[offsetRWORK + (i - ll - 1 + nm12) * strideRWORK] = cosl;
                RWORK[offsetRWORK + (i - ll - 1 + nm13) * strideRWORK] = -sinl;
              }
              e[offsetE + ll * strideE] = f;
              if (Math.abs(e[offsetE + ll * strideE]) <= thresh) {
                e[offsetE + ll * strideE] = ZERO;
              }
              if (ncvt > 0) {
                zlasr(
                  "L",
                  "V",
                  "B",
                  m - ll + 1,
                  ncvt,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm12 * strideRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm13 * strideRWORK,
                  VT,
                  strideVT1,
                  strideVT2,
                  offsetVT + ll * strideVT1
                );
              }
              if (nru > 0) {
                zlasr(
                  "R",
                  "V",
                  "B",
                  nru,
                  m - ll + 1,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
                  U,
                  strideU1,
                  strideU2,
                  offsetU + ll * strideU2
                );
              }
              if (ncc > 0) {
                zlasr(
                  "L",
                  "V",
                  "B",
                  m - ll + 1,
                  ncc,
                  RWORK,
                  strideRWORK,
                  offsetRWORK,
                  RWORK,
                  strideRWORK,
                  offsetRWORK + nm1 * strideRWORK,
                  C,
                  strideC1,
                  strideC2,
                  offsetC + ll * strideC1
                );
              }
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
            zdscal(ncvt, NEGONE, VT, strideVT2, offsetVT + i * strideVT1);
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
            zswap(
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
            zswap(
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
            zswap(
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
    module.exports = zbdsqr;
  }
});

// lib/blas/base/dznrm2/lib/base.js
var require_base20 = __commonJS({
  "lib/blas/base/dznrm2/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var TSML = 14916681462400413e-170;
    var TBIG = 1997919072202235e131;
    var SSML = 44989137945431964e145;
    var SBIG = 11113793747425387e-178;
    function dznrm2(N, zx, strideX, offsetX) {
      var notbig;
      var abig;
      var amed;
      var asml;
      var sumsq;
      var scl;
      var xv;
      var ax;
      var ix;
      var i;
      if (N <= 0) {
        return 0;
      }
      xv = reinterpret2(zx, 0);
      scl = 1;
      sumsq = 0;
      notbig = true;
      asml = 0;
      amed = 0;
      abig = 0;
      ix = offsetX * 2;
      for (i = 0; i < N; i++) {
        ax = Math.abs(xv[ix]);
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
        ax = Math.abs(xv[ix + 1]);
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
        ix += 2 * strideX;
      }
      if (abig > 0) {
        if (amed > 0 || amed !== amed) {
          abig += amed * SBIG * SBIG;
        }
        return Math.sqrt(abig) / SBIG;
      }
      if (asml > 0) {
        if (amed > 0 || amed !== amed) {
          amed = Math.sqrt(amed);
          asml = Math.sqrt(asml) / SSML;
          if (asml > amed) {
            return asml * Math.sqrt(1 + amed / asml * (amed / asml));
          }
          return amed * Math.sqrt(1 + asml / amed * (asml / amed));
        }
        return Math.sqrt(asml) / SSML;
      }
      return Math.sqrt(amed);
    }
    module.exports = dznrm2;
  }
});

// lib/blas/base/zscal/lib/base.js
var require_base21 = __commonJS({
  "lib/blas/base/zscal/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function zscal(N, za, zx, strideX, offsetX) {
      var zaR;
      var zaI;
      var xv;
      var sx;
      var ix;
      var tr;
      var i;
      if (N <= 0) {
        return zx;
      }
      zaR = real(za);
      zaI = imag(za);
      if (zaR === 1 && zaI === 0) {
        return zx;
      }
      xv = reinterpret2(zx, 0);
      ix = offsetX * 2;
      sx = strideX * 2;
      for (i = 0; i < N; i++) {
        tr = zaR * xv[ix] - zaI * xv[ix + 1];
        xv[ix + 1] = zaR * xv[ix + 1] + zaI * xv[ix];
        xv[ix] = tr;
        ix += sx;
      }
      return zx;
    }
    module.exports = zscal;
  }
});

// lib/lapack/base/dlapy3/lib/base.js
var require_base22 = __commonJS({
  "lib/lapack/base/dlapy3/lib/base.js"(exports, module) {
    "use strict";
    var HUGEVAL = 17976931348623157e292;
    function dlapy3(x, y, z) {
      var xabs;
      var yabs;
      var zabs;
      var w;
      xabs = Math.abs(x);
      yabs = Math.abs(y);
      zabs = Math.abs(z);
      w = Math.max(xabs, yabs, zabs);
      if (w === 0 || w > HUGEVAL) {
        return xabs + yabs + zabs;
      }
      return w * Math.sqrt(xabs / w * (xabs / w) + yabs / w * (yabs / w) + zabs / w * (zabs / w));
    }
    module.exports = dlapy3;
  }
});

// node_modules/@stdlib/complex/float64/base/mul/lib/main.js
var require_main55 = __commonJS({
  "node_modules/@stdlib/complex/float64/base/mul/lib/main.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var real = require_lib54();
    var imag = require_lib55();
    function mul(z1, z2) {
      var re1 = real(z1);
      var re2 = real(z2);
      var im1 = imag(z1);
      var im2 = imag(z2);
      var re = re1 * re2 - im1 * im2;
      var im = re1 * im2 + im1 * re2;
      return new Complex128(re, im);
    }
    module.exports = mul;
  }
});

// node_modules/@stdlib/complex/float64/base/mul/lib/index.js
var require_lib61 = __commonJS({
  "node_modules/@stdlib/complex/float64/base/mul/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main55();
    module.exports = main;
  }
});

// node_modules/@stdlib/complex/float64/base/add/lib/main.js
var require_main56 = __commonJS({
  "node_modules/@stdlib/complex/float64/base/add/lib/main.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var real = require_lib54();
    var imag = require_lib55();
    function cadd2(z1, z2) {
      var re = real(z1) + real(z2);
      var im = imag(z1) + imag(z2);
      return new Complex128(re, im);
    }
    module.exports = cadd2;
  }
});

// node_modules/@stdlib/complex/float64/base/add/lib/index.js
var require_lib62 = __commonJS({
  "node_modules/@stdlib/complex/float64/base/add/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main56();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/abs/lib/main.js
var require_main57 = __commonJS({
  "node_modules/@stdlib/math/base/special/abs/lib/main.js"(exports, module) {
    "use strict";
    function abs(x) {
      return Math.abs(x);
    }
    module.exports = abs;
  }
});

// node_modules/@stdlib/math/base/special/abs/lib/index.js
var require_lib63 = __commonJS({
  "node_modules/@stdlib/math/base/special/abs/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main57();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/assert/is-positive-zero/lib/main.js
var require_main58 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-positive-zero/lib/main.js"(exports, module) {
    "use strict";
    var PINF = require_lib13();
    function isPositiveZero(x) {
      return x === 0 && 1 / x === PINF;
    }
    module.exports = isPositiveZero;
  }
});

// node_modules/@stdlib/math/base/assert/is-positive-zero/lib/index.js
var require_lib64 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-positive-zero/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main58();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/assert/is-nan/lib/main.js
var require_main59 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-nan/lib/main.js"(exports, module) {
    "use strict";
    function isnan(x) {
      return x !== x;
    }
    module.exports = isnan;
  }
});

// node_modules/@stdlib/math/base/assert/is-nan/lib/index.js
var require_lib65 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-nan/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main59();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/max/lib/main.js
var require_main60 = __commonJS({
  "node_modules/@stdlib/math/base/special/max/lib/main.js"(exports, module) {
    "use strict";
    var isPositiveZero = require_lib64();
    var isnan = require_lib65();
    var PINF = require_lib13();
    function max(x, y) {
      if (isnan(x) || isnan(y)) {
        return NaN;
      }
      if (x === PINF || y === PINF) {
        return PINF;
      }
      if (x === y && x === 0) {
        if (isPositiveZero(x)) {
          return x;
        }
        return y;
      }
      if (x > y) {
        return x;
      }
      return y;
    }
    module.exports = max;
  }
});

// node_modules/@stdlib/math/base/special/max/lib/index.js
var require_lib66 = __commonJS({
  "node_modules/@stdlib/math/base/special/max/lib/index.js"(exports, module) {
    "use strict";
    var max = require_main60();
    module.exports = max;
  }
});

// node_modules/@stdlib/constants/float64/max/lib/index.js
var require_lib67 = __commonJS({
  "node_modules/@stdlib/constants/float64/max/lib/index.js"(exports, module) {
    "use strict";
    var FLOAT64_MAX = 17976931348623157e292;
    module.exports = FLOAT64_MAX;
  }
});

// node_modules/@stdlib/constants/float64/smallest-normal/lib/index.js
var require_lib68 = __commonJS({
  "node_modules/@stdlib/constants/float64/smallest-normal/lib/index.js"(exports, module) {
    "use strict";
    var FLOAT64_SMALLEST_NORMAL = 22250738585072014e-324;
    module.exports = FLOAT64_SMALLEST_NORMAL;
  }
});

// node_modules/@stdlib/constants/float64/eps/lib/index.js
var require_lib69 = __commonJS({
  "node_modules/@stdlib/constants/float64/eps/lib/index.js"(exports, module) {
    "use strict";
    var FLOAT64_EPSILON = 2220446049250313e-31;
    module.exports = FLOAT64_EPSILON;
  }
});

// node_modules/@stdlib/math/base/ops/cdiv/lib/internal_compreal.js
var require_internal_compreal = __commonJS({
  "node_modules/@stdlib/math/base/ops/cdiv/lib/internal_compreal.js"(exports, module) {
    "use strict";
    function internalCompreal(re1, im1, re2, im2, r, t) {
      var br;
      if (r === 0) {
        return (re1 + im2 * (im1 / re2)) * t;
      }
      br = im1 * r;
      if (br === 0) {
        return re1 * t + im1 * t * r;
      }
      return (re1 + br) * t;
    }
    module.exports = internalCompreal;
  }
});

// node_modules/@stdlib/math/base/ops/cdiv/lib/robust_internal.js
var require_robust_internal = __commonJS({
  "node_modules/@stdlib/math/base/ops/cdiv/lib/robust_internal.js"(exports, module) {
    "use strict";
    var internalCompreal = require_internal_compreal();
    function robustInternal(re1, im1, re2, im2) {
      var out;
      var r;
      var t;
      out = [0, 0];
      r = im2 / re2;
      t = 1 / (re2 + im2 * r);
      out[0] = internalCompreal(re1, im1, re2, im2, r, t);
      out[1] = internalCompreal(im1, -re1, re2, im2, r, t);
      return out;
    }
    module.exports = robustInternal;
  }
});

// node_modules/@stdlib/math/base/ops/cdiv/lib/main.js
var require_main61 = __commonJS({
  "node_modules/@stdlib/math/base/ops/cdiv/lib/main.js"(exports, module) {
    "use strict";
    var abs = require_lib63();
    var max = require_lib66();
    var FLOAT64_BIGGEST = require_lib67();
    var FLOAT64_SMALLEST = require_lib68();
    var EPS = require_lib69();
    var real = require_lib54();
    var imag = require_lib55();
    var Complex128 = require_lib38();
    var robustInternal = require_robust_internal();
    var LARGE_THRESHOLD = FLOAT64_BIGGEST * 0.5;
    var SMALL_THRESHOLD = FLOAT64_SMALLEST * (2 / EPS);
    var RECIP_EPS_SQR = 2 / (EPS * EPS);
    function cdiv(z1, z2) {
      var re1;
      var re2;
      var im1;
      var im2;
      var out;
      var ab;
      var cd;
      var s;
      re1 = real(z1);
      re2 = real(z2);
      im1 = imag(z1);
      im2 = imag(z2);
      ab = max(abs(re1), abs(im1));
      cd = max(abs(re2), abs(im2));
      s = 1;
      if (ab >= LARGE_THRESHOLD) {
        re1 *= 0.5;
        im1 *= 0.5;
        s *= 2;
      } else if (ab <= SMALL_THRESHOLD) {
        re1 *= RECIP_EPS_SQR;
        im1 *= RECIP_EPS_SQR;
        s /= RECIP_EPS_SQR;
      }
      if (cd >= LARGE_THRESHOLD) {
        re2 *= 0.5;
        im2 *= 0.5;
        s *= 0.5;
      } else if (cd <= SMALL_THRESHOLD) {
        re2 *= RECIP_EPS_SQR;
        im2 *= RECIP_EPS_SQR;
        s *= RECIP_EPS_SQR;
      }
      if (abs(im2) <= abs(re2)) {
        out = robustInternal(re1, im1, re2, im2);
      } else {
        out = robustInternal(im1, re1, im2, re2);
        out[1] *= -1;
      }
      out[0] *= s;
      out[1] *= s;
      return new Complex128(out[0], out[1]);
    }
    module.exports = cdiv;
  }
});

// node_modules/@stdlib/math/base/ops/cdiv/lib/index.js
var require_lib70 = __commonJS({
  "node_modules/@stdlib/math/base/ops/cdiv/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main61();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/ops/csub/lib/main.js
var require_main62 = __commonJS({
  "node_modules/@stdlib/math/base/ops/csub/lib/main.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var real = require_lib54();
    var imag = require_lib55();
    function csub2(z1, z2) {
      var re = real(z1) - real(z2);
      var im = imag(z1) - imag(z2);
      return new Complex128(re, im);
    }
    module.exports = csub2;
  }
});

// node_modules/@stdlib/math/base/ops/csub/lib/index.js
var require_lib71 = __commonJS({
  "node_modules/@stdlib/math/base/ops/csub/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main62();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/ops/cneg/lib/main.js
var require_main63 = __commonJS({
  "node_modules/@stdlib/math/base/ops/cneg/lib/main.js"(exports, module) {
    "use strict";
    var real = require_lib54();
    var imag = require_lib55();
    var Complex128 = require_lib38();
    function cneg(z) {
      return new Complex128(-real(z), -imag(z));
    }
    module.exports = cneg;
  }
});

// node_modules/@stdlib/math/base/ops/cneg/lib/index.js
var require_lib72 = __commonJS({
  "node_modules/@stdlib/math/base/ops/cneg/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main63();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/assert/is-infinite/lib/main.js
var require_main64 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-infinite/lib/main.js"(exports, module) {
    "use strict";
    var PINF = require_lib13();
    var NINF = require_lib14();
    function isInfinite(x) {
      return x === PINF || x === NINF;
    }
    module.exports = isInfinite;
  }
});

// node_modules/@stdlib/math/base/assert/is-infinite/lib/index.js
var require_lib73 = __commonJS({
  "node_modules/@stdlib/math/base/assert/is-infinite/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main64();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/sqrt/lib/main.js
var require_main65 = __commonJS({
  "node_modules/@stdlib/math/base/special/sqrt/lib/main.js"(exports, module) {
    "use strict";
    var sqrt = Math.sqrt;
    module.exports = sqrt;
  }
});

// node_modules/@stdlib/math/base/special/sqrt/lib/index.js
var require_lib74 = __commonJS({
  "node_modules/@stdlib/math/base/special/sqrt/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main65();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/hypot/lib/main.js
var require_main66 = __commonJS({
  "node_modules/@stdlib/math/base/special/hypot/lib/main.js"(exports, module) {
    "use strict";
    var isnan = require_lib65();
    var isInfinite = require_lib73();
    var PINF = require_lib13();
    var sqrt = require_lib74();
    function hypot(x, y) {
      var tmp;
      if (isnan(x) || isnan(y)) {
        return NaN;
      }
      if (isInfinite(x) || isInfinite(y)) {
        return PINF;
      }
      if (x < 0) {
        x = -x;
      }
      if (y < 0) {
        y = -y;
      }
      if (x < y) {
        tmp = y;
        y = x;
        x = tmp;
      }
      if (x === 0) {
        return 0;
      }
      y /= x;
      return x * sqrt(1 + y * y);
    }
    module.exports = hypot;
  }
});

// node_modules/@stdlib/math/base/special/hypot/lib/index.js
var require_lib75 = __commonJS({
  "node_modules/@stdlib/math/base/special/hypot/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main66();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/cabs/lib/main.js
var require_main67 = __commonJS({
  "node_modules/@stdlib/math/base/special/cabs/lib/main.js"(exports, module) {
    "use strict";
    var hypot = require_lib75();
    var real = require_lib54();
    var imag = require_lib55();
    function cabs2(z) {
      return hypot(real(z), imag(z));
    }
    module.exports = cabs2;
  }
});

// node_modules/@stdlib/math/base/special/cabs/lib/index.js
var require_lib76 = __commonJS({
  "node_modules/@stdlib/math/base/special/cabs/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main67();
    module.exports = main;
  }
});

// node_modules/@stdlib/math/base/special/cabs2/lib/main.js
var require_main68 = __commonJS({
  "node_modules/@stdlib/math/base/special/cabs2/lib/main.js"(exports, module) {
    "use strict";
    var real = require_lib54();
    var imag = require_lib55();
    function cabs2(z) {
      var re = real(z);
      var im = imag(z);
      return re * re + im * im;
    }
    module.exports = cabs2;
  }
});

// node_modules/@stdlib/math/base/special/cabs2/lib/index.js
var require_lib77 = __commonJS({
  "node_modules/@stdlib/math/base/special/cabs2/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main68();
    module.exports = main;
  }
});

// node_modules/@stdlib/complex/float64/conj/lib/main.js
var require_main69 = __commonJS({
  "node_modules/@stdlib/complex/float64/conj/lib/main.js"(exports, module) {
    "use strict";
    function conj(z) {
      return new z.constructor(z.re, -z.im);
    }
    module.exports = conj;
  }
});

// node_modules/@stdlib/complex/float64/conj/lib/index.js
var require_lib78 = __commonJS({
  "node_modules/@stdlib/complex/float64/conj/lib/index.js"(exports, module) {
    "use strict";
    var main = require_main69();
    module.exports = main;
  }
});

// lib/cmplx.js
var require_cmplx = __commonJS({
  "lib/cmplx.js"(exports, module) {
    "use strict";
    var cmul2 = require_lib61();
    var cadd2 = require_lib62();
    var cdiv = require_lib70();
    var csub2 = require_lib71();
    var cneg = require_lib72();
    var cabs2 = require_lib76();
    var cabs22 = require_lib77();
    var conj = require_lib78();
    var real = require_lib54();
    var imag = require_lib55();
    function cabsAt(arr, idx) {
      var ar = Math.abs(arr[idx]);
      var ai = Math.abs(arr[idx + 1]);
      var mx;
      var mn;
      var r;
      if (ar === 0 && ai === 0) {
        return 0;
      }
      if (ar >= ai) {
        mx = ar;
        mn = ai;
      } else {
        mx = ai;
        mn = ar;
      }
      r = mn / mx;
      return mx * Math.sqrt(1 + r * r);
    }
    function cabs1At(arr, idx) {
      return Math.abs(arr[idx]) + Math.abs(arr[idx + 1]);
    }
    function cmulAt(out, oi, a, ai, b, bi) {
      var ar = a[ai];
      var aim = a[ai + 1];
      var br = b[bi];
      var bim = b[bi + 1];
      out[oi] = ar * br - aim * bim;
      out[oi + 1] = ar * bim + aim * br;
    }
    function cdivAt(out, oi, a, ai, b, bi) {
      var ar = a[ai];
      var aim = a[ai + 1];
      var br = b[bi];
      var bim = b[bi + 1];
      var r;
      var d;
      if (Math.abs(bim) <= Math.abs(br)) {
        r = bim / br;
        d = br + bim * r;
        out[oi] = (ar + aim * r) / d;
        out[oi + 1] = (aim - ar * r) / d;
      } else {
        r = br / bim;
        d = bim + br * r;
        out[oi] = (ar * r + aim) / d;
        out[oi + 1] = (aim * r - ar) / d;
      }
    }
    module.exports = {
      // Scalar operations (stdlib re-exports, operate on Complex128 objects)
      mul: cmul2,
      add: cadd2,
      div: cdiv,
      sub: csub2,
      neg: cneg,
      abs: cabs2,
      abs2: cabs22,
      conj,
      real,
      imag,
      // Indexed operations (operate on Float64Array views at specific indices)
      absAt: cabsAt,
      abs1At: cabs1At,
      mulAt: cmulAt,
      divAt: cdivAt
    };
  }
});

// lib/lapack/base/zlarfg/lib/base.js
var require_base23 = __commonJS({
  "lib/lapack/base/zlarfg/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var Complex128Array2 = require_lib60();
    var reinterpret2 = require_lib57();
    var dznrm2 = require_base20();
    var zdscal = require_base16();
    var zscal = require_base21();
    var dlamch = require_base();
    var dlapy3 = require_base22();
    var cmplx = require_cmplx();
    var SCRATCH = new Float64Array(4);
    var SCRATCH_CA = new Complex128Array2(1);
    var SCRATCH_CAv = reinterpret2(SCRATCH_CA, 0);
    function zlarfg(N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau) {
      var rsafmn;
      var safmin;
      var alphr;
      var alphi;
      var xnorm;
      var beta;
      var tauv;
      var tmp;
      var av;
      var oA;
      var oT;
      var knt;
      var j;
      tauv = reinterpret2(tau, 0);
      oT = offsetTau * 2;
      if (N <= 0) {
        tauv[oT] = 0;
        tauv[oT + 1] = 0;
        return;
      }
      av = reinterpret2(alpha, 0);
      oA = offsetAlpha * 2;
      xnorm = dznrm2(N - 1, x, strideX, offsetX);
      alphr = av[oA];
      alphi = av[oA + 1];
      if (xnorm === 0 && alphi === 0) {
        tauv[oT] = 0;
        tauv[oT + 1] = 0;
      } else {
        beta = -(Math.sign(alphr) || 1) * dlapy3(alphr, alphi, xnorm);
        safmin = dlamch("S") / dlamch("E");
        rsafmn = 1 / safmin;
        knt = 0;
        if (Math.abs(beta) < safmin) {
          do {
            knt = knt + 1;
            zdscal(N - 1, rsafmn, x, strideX, offsetX);
            beta = beta * rsafmn;
            alphi = alphi * rsafmn;
            alphr = alphr * rsafmn;
          } while (Math.abs(beta) < safmin && knt < 20);
          xnorm = dznrm2(N - 1, x, strideX, offsetX);
          av[oA] = alphr;
          av[oA + 1] = alphi;
          beta = -(Math.sign(alphr) || 1) * dlapy3(alphr, alphi, xnorm);
        }
        tauv[oT] = (beta - alphr) / beta;
        tauv[oT + 1] = -alphi / beta;
        SCRATCH[0] = 1;
        SCRATCH[1] = 0;
        SCRATCH[2] = alphr - beta;
        SCRATCH[3] = alphi;
        cmplx.divAt(SCRATCH, 0, SCRATCH, 0, SCRATCH, 2);
        SCRATCH_CAv[0] = SCRATCH[0];
        SCRATCH_CAv[1] = SCRATCH[1];
        zscal(N - 1, SCRATCH_CA.get(0), x, strideX, offsetX);
        for (j = 0; j < knt; j++) {
          beta = beta * safmin;
        }
        av[oA] = beta;
        av[oA + 1] = 0;
      }
    }
    module.exports = zlarfg;
  }
});

// lib/blas/base/zgemv/lib/base.js
var require_base24 = __commonJS({
  "lib/blas/base/zgemv/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function zgemv(trans, M, N, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY) {
      var noTrans;
      var noConj;
      var alphaR;
      var alphaI;
      var betaR;
      var betaI;
      var tempR;
      var tempI;
      var lenx;
      var leny;
      var aijR;
      var aijI;
      var sa1;
      var sa2;
      var sx;
      var sy;
      var ix;
      var iy;
      var jx;
      var jy;
      var ai;
      var oA;
      var oX;
      var oY;
      var Av;
      var xv;
      var yv;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return y;
      }
      alphaR = real(alpha);
      alphaI = imag(alpha);
      betaR = real(beta);
      betaI = imag(beta);
      if (alphaR === 0 && alphaI === 0 && betaR === 1 && betaI === 0) {
        return y;
      }
      noTrans = trans === "N" || trans === "n";
      noConj = trans === "T" || trans === "t";
      if (noTrans) {
        lenx = N;
        leny = M;
      } else {
        lenx = M;
        leny = N;
      }
      Av = reinterpret2(A, 0);
      oA = offsetA * 2;
      xv = reinterpret2(x, 0);
      oX = offsetX * 2;
      yv = reinterpret2(y, 0);
      oY = offsetY * 2;
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      sx = strideX * 2;
      sy = strideY * 2;
      if (betaR !== 1 || betaI !== 0) {
        iy = oY;
        if (betaR === 0 && betaI === 0) {
          for (i = 0; i < leny; i++) {
            yv[iy] = 0;
            yv[iy + 1] = 0;
            iy += sy;
          }
        } else {
          for (i = 0; i < leny; i++) {
            tempR = betaR * yv[iy] - betaI * yv[iy + 1];
            tempI = betaR * yv[iy + 1] + betaI * yv[iy];
            yv[iy] = tempR;
            yv[iy + 1] = tempI;
            iy += sy;
          }
        }
      }
      if (alphaR === 0 && alphaI === 0) {
        return y;
      }
      if (noTrans) {
        jx = oX;
        for (j = 0; j < N; j++) {
          tempR = alphaR * xv[jx] - alphaI * xv[jx + 1];
          tempI = alphaR * xv[jx + 1] + alphaI * xv[jx];
          iy = oY;
          ai = oA + j * sa2;
          for (i = 0; i < M; i++) {
            aijR = Av[ai];
            aijI = Av[ai + 1];
            yv[iy] += tempR * aijR - tempI * aijI;
            yv[iy + 1] += tempR * aijI + tempI * aijR;
            iy += sy;
            ai += sa1;
          }
          jx += sx;
        }
      } else {
        jy = oY;
        for (j = 0; j < N; j++) {
          tempR = 0;
          tempI = 0;
          ix = oX;
          ai = oA + j * sa2;
          if (noConj) {
            for (i = 0; i < M; i++) {
              aijR = Av[ai];
              aijI = Av[ai + 1];
              tempR += aijR * xv[ix] - aijI * xv[ix + 1];
              tempI += aijR * xv[ix + 1] + aijI * xv[ix];
              ix += sx;
              ai += sa1;
            }
          } else {
            for (i = 0; i < M; i++) {
              aijR = Av[ai];
              aijI = -Av[ai + 1];
              tempR += aijR * xv[ix] - aijI * xv[ix + 1];
              tempI += aijR * xv[ix + 1] + aijI * xv[ix];
              ix += sx;
              ai += sa1;
            }
          }
          yv[jy] += alphaR * tempR - alphaI * tempI;
          yv[jy + 1] += alphaR * tempI + alphaI * tempR;
          jy += sy;
        }
      }
      return y;
    }
    module.exports = zgemv;
  }
});

// lib/blas/base/zgerc/lib/base.js
var require_base25 = __commonJS({
  "lib/blas/base/zgerc/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function zgerc(M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA) {
      var alphaRe;
      var alphaIm;
      var sa1;
      var sa2;
      var sx;
      var sy;
      var ix;
      var jy;
      var ia;
      var oA;
      var oX;
      var oY;
      var Av;
      var xv;
      var yv;
      var tr;
      var ti;
      var yr;
      var yi;
      var i;
      var j;
      if (M <= 0 || N <= 0) {
        return A;
      }
      alphaRe = real(alpha);
      alphaIm = imag(alpha);
      if (alphaRe === 0 && alphaIm === 0) {
        return A;
      }
      Av = reinterpret2(A, 0);
      oA = offsetA * 2;
      xv = reinterpret2(x, 0);
      oX = offsetX * 2;
      yv = reinterpret2(y, 0);
      oY = offsetY * 2;
      sx = strideX * 2;
      sy = strideY * 2;
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      jy = oY;
      for (j = 0; j < N; j++) {
        yr = yv[jy];
        yi = yv[jy + 1];
        if (yr !== 0 || yi !== 0) {
          tr = alphaRe * yr + alphaIm * yi;
          ti = alphaIm * yr - alphaRe * yi;
          ix = oX;
          ia = oA + j * sa2;
          for (i = 0; i < M; i++) {
            Av[ia] += xv[ix] * tr - xv[ix + 1] * ti;
            Av[ia + 1] += xv[ix] * ti + xv[ix + 1] * tr;
            ix += sx;
            ia += sa1;
          }
        }
        jy += sy;
      }
      return A;
    }
    module.exports = zgerc;
  }
});

// lib/lapack/base/ilazlr/lib/base.js
var require_base26 = __commonJS({
  "lib/lapack/base/ilazlr/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function ilazlr(M, N, A, strideA1, strideA2, offsetA) {
      var result;
      var Av;
      var sa1;
      var sa2;
      var oA;
      var re;
      var im;
      var i;
      var j;
      if (M === 0) {
        return -1;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      re = Av[oA + (M - 1) * sa1 + 0 * sa2];
      im = Av[oA + (M - 1) * sa1 + 0 * sa2 + 1];
      if (re !== 0 || im !== 0) {
        return M - 1;
      }
      re = Av[oA + (M - 1) * sa1 + (N - 1) * sa2];
      im = Av[oA + (M - 1) * sa1 + (N - 1) * sa2 + 1];
      if (re !== 0 || im !== 0) {
        return M - 1;
      }
      result = -1;
      for (j = 0; j < N; j++) {
        i = M - 1;
        while (i >= 0) {
          re = Av[oA + i * sa1 + j * sa2];
          im = Av[oA + i * sa1 + j * sa2 + 1];
          if (re !== 0 || im !== 0) {
            break;
          }
          i--;
        }
        if (i > result) {
          result = i;
        }
      }
      return result;
    }
    module.exports = ilazlr;
  }
});

// lib/lapack/base/ilazlc/lib/base.js
var require_base27 = __commonJS({
  "lib/lapack/base/ilazlc/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function ilazlc(M, N, A, strideA1, strideA2, offsetA) {
      var Av;
      var sa1;
      var sa2;
      var oA;
      var re;
      var im;
      var i;
      var j;
      if (N === 0) {
        return -1;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      re = Av[oA + 0 * sa1 + (N - 1) * sa2];
      im = Av[oA + 0 * sa1 + (N - 1) * sa2 + 1];
      if (re !== 0 || im !== 0) {
        return N - 1;
      }
      re = Av[oA + (M - 1) * sa1 + (N - 1) * sa2];
      im = Av[oA + (M - 1) * sa1 + (N - 1) * sa2 + 1];
      if (re !== 0 || im !== 0) {
        return N - 1;
      }
      for (j = N - 1; j >= 0; j--) {
        for (i = 0; i < M; i++) {
          re = Av[oA + i * sa1 + j * sa2];
          im = Av[oA + i * sa1 + j * sa2 + 1];
          if (re !== 0 || im !== 0) {
            return j;
          }
        }
      }
      return -1;
    }
    module.exports = ilazlc;
  }
});

// lib/lapack/base/zlarf/lib/base.js
var require_base28 = __commonJS({
  "lib/lapack/base/zlarf/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zgemv = require_base24();
    var zgerc = require_base25();
    var ilazlr = require_base26();
    var ilazlc = require_base27();
    var ONE = new Complex128(1, 0);
    var ZERO = new Complex128(0, 0);
    function zlarf(side, M, N, v, strideV, offsetV, tau, offsetTau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK) {
      var applyLeft;
      var negTau;
      var lastv;
      var lastc;
      var tauR;
      var tauI;
      var tauv;
      var vv;
      var sv;
      var ix;
      var oT;
      tauv = reinterpret2(tau, 0);
      oT = offsetTau * 2;
      tauR = tauv[oT];
      tauI = tauv[oT + 1];
      applyLeft = side === "L" || side === "l";
      lastv = 0;
      lastc = 0;
      if (tauR !== 0 || tauI !== 0) {
        if (applyLeft) {
          lastv = M;
        } else {
          lastv = N;
        }
        vv = reinterpret2(v, 0);
        sv = strideV * 2;
        if (strideV > 0) {
          ix = offsetV * 2 + (lastv - 1) * sv;
        } else {
          ix = offsetV * 2;
        }
        while (lastv > 0 && vv[ix] === 0 && vv[ix + 1] === 0) {
          lastv -= 1;
          ix -= sv;
        }
        if (applyLeft) {
          lastc = ilazlc(lastv, N, C, strideC1, strideC2, offsetC) + 1;
        } else {
          lastc = ilazlr(M, lastv, C, strideC1, strideC2, offsetC) + 1;
        }
      }
      negTau = new Complex128(-tauR, -tauI);
      if (applyLeft) {
        if (lastv > 0) {
          zgemv(
            "C",
            lastv,
            lastc,
            ONE,
            C,
            strideC1,
            strideC2,
            offsetC,
            v,
            strideV,
            offsetV,
            ZERO,
            WORK,
            strideWORK,
            offsetWORK
          );
          zgerc(
            lastv,
            lastc,
            negTau,
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
      } else {
        if (lastv > 0) {
          zgemv(
            "N",
            lastc,
            lastv,
            ONE,
            C,
            strideC1,
            strideC2,
            offsetC,
            v,
            strideV,
            offsetV,
            ZERO,
            WORK,
            strideWORK,
            offsetWORK
          );
          zgerc(
            lastc,
            lastv,
            negTau,
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
    }
    module.exports = zlarf;
  }
});

// lib/lapack/base/zlacgv/lib/base.js
var require_base29 = __commonJS({
  "lib/lapack/base/zlacgv/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zlacgv(N, x, stride, offset) {
      var xv;
      var sx;
      var ix;
      var i;
      if (N <= 0) {
        return x;
      }
      xv = reinterpret2(x, 0);
      ix = offset * 2;
      sx = stride * 2;
      for (i = 0; i < N; i++) {
        xv[ix + 1] = -xv[ix + 1];
        ix += sx;
      }
      return x;
    }
    module.exports = zlacgv;
  }
});

// lib/lapack/base/zgebd2/lib/base.js
var require_base30 = __commonJS({
  "lib/lapack/base/zgebd2/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var reinterpret2 = require_lib57();
    var zlarfg = require_base23();
    var zlarf = require_base28();
    var zlacgv = require_base29();
    function zgebd2(M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK) {
      var conj_tauq;
      var conj_f64;
      var tauq_f64;
      var taup_f64;
      var tauq_off;
      var taup_off;
      var sa1;
      var sa2;
      var oA;
      var Av;
      var aii;
      var aij;
      var i;
      Av = reinterpret2(A, 0);
      tauq_f64 = reinterpret2(TAUQ, 0);
      taup_f64 = reinterpret2(TAUP, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      conj_tauq = new Complex128Array2(1);
      conj_f64 = reinterpret2(conj_tauq, 0);
      if (M >= N) {
        for (i = 0; i < N; i++) {
          aii = oA + i * sa1 + i * sa2;
          tauq_off = offsetTAUQ + i * strideTAUQ;
          zlarfg(
            M - i,
            A,
            offsetA + i * strideA1 + i * strideA2,
            A,
            strideA1,
            offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
            TAUQ,
            tauq_off
          );
          d[offsetD + i * strideD] = Av[aii];
          Av[aii] = 1;
          Av[aii + 1] = 0;
          if (i < N - 1) {
            conj_f64[0] = tauq_f64[(offsetTAUQ + i * strideTAUQ) * 2];
            conj_f64[1] = -tauq_f64[(offsetTAUQ + i * strideTAUQ) * 2 + 1];
            zlarf(
              "L",
              M - i,
              N - i - 1,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              conj_tauq,
              0,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          Av[aii] = d[offsetD + i * strideD];
          Av[aii + 1] = 0;
          if (i < N - 1) {
            taup_off = offsetTAUP + i * strideTAUP;
            aij = oA + i * sa1 + (i + 1) * sa2;
            zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
            zlarfg(
              N - i - 1,
              A,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + Math.min(i + 2, N - 1) * strideA2,
              TAUP,
              taup_off
            );
            e[offsetE + i * strideE] = Av[aij];
            Av[aij] = 1;
            Av[aij + 1] = 0;
            zlarf(
              "R",
              M - i - 1,
              N - i - 1,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              TAUP,
              taup_off,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
            zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
            Av[aij] = e[offsetE + i * strideE];
            Av[aij + 1] = 0;
          } else {
            taup_off = (offsetTAUP + i * strideTAUP) * 2;
            taup_f64[taup_off] = 0;
            taup_f64[taup_off + 1] = 0;
          }
        }
      } else {
        for (i = 0; i < M; i++) {
          aii = oA + i * sa1 + i * sa2;
          taup_off = offsetTAUP + i * strideTAUP;
          zlacgv(N - i, A, strideA2, offsetA + i * strideA1 + i * strideA2);
          zlarfg(
            N - i,
            A,
            offsetA + i * strideA1 + i * strideA2,
            A,
            strideA2,
            offsetA + i * strideA1 + Math.min(i + 1, N - 1) * strideA2,
            TAUP,
            taup_off
          );
          d[offsetD + i * strideD] = Av[aii];
          Av[aii] = 1;
          Av[aii + 1] = 0;
          if (i < M - 1) {
            zlarf(
              "R",
              M - i - 1,
              N - i,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              TAUP,
              taup_off,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          zlacgv(N - i, A, strideA2, offsetA + i * strideA1 + i * strideA2);
          Av[aii] = d[offsetD + i * strideD];
          Av[aii + 1] = 0;
          if (i < M - 1) {
            tauq_off = offsetTAUQ + i * strideTAUQ;
            aij = oA + (i + 1) * sa1 + i * sa2;
            zlarfg(
              M - i - 1,
              A,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              A,
              strideA1,
              offsetA + Math.min(i + 2, M - 1) * strideA1 + i * strideA2,
              TAUQ,
              tauq_off
            );
            e[offsetE + i * strideE] = Av[aij];
            Av[aij] = 1;
            Av[aij + 1] = 0;
            conj_f64[0] = tauq_f64[(offsetTAUQ + i * strideTAUQ) * 2];
            conj_f64[1] = -tauq_f64[(offsetTAUQ + i * strideTAUQ) * 2 + 1];
            zlarf(
              "L",
              M - i - 1,
              N - i - 1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              conj_tauq,
              0,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
            Av[aij] = e[offsetE + i * strideE];
            Av[aij + 1] = 0;
          } else {
            tauq_off = (offsetTAUQ + i * strideTAUQ) * 2;
            tauq_f64[tauq_off] = 0;
            tauq_f64[tauq_off + 1] = 0;
          }
        }
      }
      return 0;
    }
    module.exports = zgebd2;
  }
});

// lib/blas/base/zgemm/lib/base.js
var require_base31 = __commonJS({
  "lib/blas/base/zgemm/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function zgemm(transa, transb, M, N, K, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC) {
      var alphaR;
      var alphaI;
      var betaR;
      var betaI;
      var tempR;
      var tempI;
      var nota;
      var notb;
      var conja;
      var conjb;
      var cR;
      var cI;
      var sa1;
      var sa2;
      var sb1;
      var sb2;
      var sc1;
      var sc2;
      var ci;
      var ai;
      var bi;
      var oA;
      var oB;
      var oC;
      var Av;
      var Bv;
      var Cv;
      var aR;
      var aI;
      var bR;
      var bI;
      var i;
      var j;
      var l;
      if (M === 0 || N === 0) {
        return C;
      }
      alphaR = real(alpha);
      alphaI = imag(alpha);
      betaR = real(beta);
      betaI = imag(beta);
      nota = transa === "N" || transa === "n";
      notb = transb === "N" || transb === "n";
      conja = transa === "C" || transa === "c";
      conjb = transb === "C" || transb === "c";
      if (alphaR === 0 && alphaI === 0 && betaR === 1 && betaI === 0) {
        return C;
      }
      Av = reinterpret2(A, 0);
      oA = offsetA * 2;
      Bv = reinterpret2(B, 0);
      oB = offsetB * 2;
      Cv = reinterpret2(C, 0);
      oC = offsetC * 2;
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      sb1 = strideB1 * 2;
      sb2 = strideB2 * 2;
      sc1 = strideC1 * 2;
      sc2 = strideC2 * 2;
      if (alphaR === 0 && alphaI === 0) {
        if (betaR === 0 && betaI === 0) {
          for (j = 0; j < N; j++) {
            ci = oC + j * sc2;
            for (i = 0; i < M; i++) {
              Cv[ci] = 0;
              Cv[ci + 1] = 0;
              ci += sc1;
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            ci = oC + j * sc2;
            for (i = 0; i < M; i++) {
              tempR = betaR * Cv[ci] - betaI * Cv[ci + 1];
              tempI = betaR * Cv[ci + 1] + betaI * Cv[ci];
              Cv[ci] = tempR;
              Cv[ci + 1] = tempI;
              ci += sc1;
            }
          }
        }
        return C;
      }
      if (notb) {
        if (nota) {
          for (j = 0; j < N; j++) {
            if (betaR === 0 && betaI === 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                Cv[ci] = 0;
                Cv[ci + 1] = 0;
                ci += sc1;
              }
            } else if (betaR !== 1 || betaI !== 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                tempR = betaR * Cv[ci] - betaI * Cv[ci + 1];
                tempI = betaR * Cv[ci + 1] + betaI * Cv[ci];
                Cv[ci] = tempR;
                Cv[ci + 1] = tempI;
                ci += sc1;
              }
            }
            for (l = 0; l < K; l++) {
              bi = oB + l * sb1 + j * sb2;
              bR = Bv[bi];
              bI = Bv[bi + 1];
              tempR = alphaR * bR - alphaI * bI;
              tempI = alphaR * bI + alphaI * bR;
              ai = oA + l * sa2;
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                Cv[ci] += tempR * aR - tempI * aI;
                Cv[ci + 1] += tempR * aI + tempI * aR;
                ai += sa1;
                ci += sc1;
              }
            }
          }
        } else if (conja) {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb2;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = -Av[ai + 1];
                bR = Bv[bi];
                bI = Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb1;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb2;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                bR = Bv[bi];
                bI = Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb1;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        }
      } else if (nota) {
        if (conjb) {
          for (j = 0; j < N; j++) {
            if (betaR === 0 && betaI === 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                Cv[ci] = 0;
                Cv[ci + 1] = 0;
                ci += sc1;
              }
            } else if (betaR !== 1 || betaI !== 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                tempR = betaR * Cv[ci] - betaI * Cv[ci + 1];
                tempI = betaR * Cv[ci + 1] + betaI * Cv[ci];
                Cv[ci] = tempR;
                Cv[ci + 1] = tempI;
                ci += sc1;
              }
            }
            for (l = 0; l < K; l++) {
              bi = oB + j * sb1 + l * sb2;
              bR = Bv[bi];
              bI = -Bv[bi + 1];
              tempR = alphaR * bR - alphaI * bI;
              tempI = alphaR * bI + alphaI * bR;
              ai = oA + l * sa2;
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                Cv[ci] += tempR * aR - tempI * aI;
                Cv[ci + 1] += tempR * aI + tempI * aR;
                ai += sa1;
                ci += sc1;
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            if (betaR === 0 && betaI === 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                Cv[ci] = 0;
                Cv[ci + 1] = 0;
                ci += sc1;
              }
            } else if (betaR !== 1 || betaI !== 0) {
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                tempR = betaR * Cv[ci] - betaI * Cv[ci + 1];
                tempI = betaR * Cv[ci + 1] + betaI * Cv[ci];
                Cv[ci] = tempR;
                Cv[ci + 1] = tempI;
                ci += sc1;
              }
            }
            for (l = 0; l < K; l++) {
              bi = oB + j * sb1 + l * sb2;
              bR = Bv[bi];
              bI = Bv[bi + 1];
              tempR = alphaR * bR - alphaI * bI;
              tempI = alphaR * bI + alphaI * bR;
              ai = oA + l * sa2;
              ci = oC + j * sc2;
              for (i = 0; i < M; i++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                Cv[ci] += tempR * aR - tempI * aI;
                Cv[ci + 1] += tempR * aI + tempI * aR;
                ai += sa1;
                ci += sc1;
              }
            }
          }
        }
      } else if (conja) {
        if (conjb) {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb1;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = -Av[ai + 1];
                bR = Bv[bi];
                bI = -Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb2;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb1;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = -Av[ai + 1];
                bR = Bv[bi];
                bI = Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb2;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        }
      } else {
        if (conjb) {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb1;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                bR = Bv[bi];
                bI = -Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb2;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        } else {
          for (j = 0; j < N; j++) {
            for (i = 0; i < M; i++) {
              tempR = 0;
              tempI = 0;
              ai = oA + i * sa2;
              bi = oB + j * sb1;
              for (l = 0; l < K; l++) {
                aR = Av[ai];
                aI = Av[ai + 1];
                bR = Bv[bi];
                bI = Bv[bi + 1];
                tempR += aR * bR - aI * bI;
                tempI += aR * bI + aI * bR;
                ai += sa1;
                bi += sb2;
              }
              ci = oC + i * sc1 + j * sc2;
              if (betaR === 0 && betaI === 0) {
                Cv[ci] = alphaR * tempR - alphaI * tempI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR;
              } else {
                cR = Cv[ci];
                cI = Cv[ci + 1];
                Cv[ci] = alphaR * tempR - alphaI * tempI + betaR * cR - betaI * cI;
                Cv[ci + 1] = alphaR * tempI + alphaI * tempR + betaR * cI + betaI * cR;
              }
            }
          }
        }
      }
      return C;
    }
    module.exports = zgemm;
  }
});

// lib/lapack/base/zlabrd/lib/base.js
var require_base32 = __commonJS({
  "lib/lapack/base/zlabrd/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zgemv = require_base24();
    var zlacgv = require_base29();
    var zlarfg = require_base23();
    var zscal = require_base21();
    var ONE = new Complex128(1, 0);
    var ZERO = new Complex128(0, 0);
    var NEGONE = new Complex128(-1, 0);
    function zlabrd(M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY) {
      var alphaRe;
      var alphaIm;
      var alpha;
      var Av;
      var sa1;
      var sa2;
      var ia;
      var i;
      alpha = new Complex128Array2(1);
      if (M <= 0 || N <= 0) {
        return;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      if (M >= N) {
        for (i = 0; i < nb; i++) {
          zlacgv(i, Y, strideY2, offsetY + i * strideY1);
          zgemv(
            "N",
            M - i,
            i,
            NEGONE,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1,
            Y,
            strideY2,
            offsetY + i * strideY1,
            ONE,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2
          );
          zlacgv(i, Y, strideY2, offsetY + i * strideY1);
          zgemv(
            "N",
            M - i,
            i,
            NEGONE,
            X,
            strideX1,
            strideX2,
            offsetX + i * strideX1,
            A,
            strideA1,
            offsetA + i * strideA2,
            ONE,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2
          );
          ia = offsetA * 2 + i * sa1 + i * sa2;
          alphaRe = Av[ia];
          alphaIm = Av[ia + 1];
          reinterpret2(alpha, 0)[0] = alphaRe;
          reinterpret2(alpha, 0)[1] = alphaIm;
          zlarfg(
            M - i,
            alpha,
            0,
            A,
            strideA1,
            offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
            TAUQ,
            offsetTAUQ + i * strideTAUQ
          );
          d[offsetD + i * strideD] = reinterpret2(alpha, 0)[0];
          if (i < N - 1) {
            Av[ia] = 1;
            Av[ia + 1] = 0;
            zgemv(
              "C",
              M - i,
              N - i - 1,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zgemv(
              "C",
              M - i,
              i,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            zgemv(
              "N",
              N - i - 1,
              i,
              NEGONE,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              Y,
              strideY1,
              offsetY + i * strideY2,
              ONE,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zgemv(
              "C",
              M - i,
              i,
              ONE,
              X,
              strideX1,
              strideX2,
              offsetX + i * strideX1,
              A,
              strideA1,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            zgemv(
              "C",
              i,
              N - i - 1,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              Y,
              strideY1,
              offsetY + i * strideY2,
              ONE,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zscal(
              N - i - 1,
              TAUQ.get(offsetTAUQ + i * strideTAUQ),
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
            zlacgv(i + 1, A, strideA2, offsetA + i * strideA1);
            zgemv(
              "N",
              N - i - 1,
              i + 1,
              NEGONE,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1,
              ONE,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2
            );
            zlacgv(i + 1, A, strideA2, offsetA + i * strideA1);
            zlacgv(i, X, strideX2, offsetX + i * strideX1);
            zgemv(
              "C",
              i,
              N - i - 1,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              X,
              strideX2,
              offsetX + i * strideX1,
              ONE,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2
            );
            zlacgv(i, X, strideX2, offsetX + i * strideX1);
            zlarfg(
              N - i - 1,
              A,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + Math.min(i + 2, N - 1) * strideA2,
              TAUP,
              offsetTAUP + i * strideTAUP
            );
            e[offsetE + i * strideE] = Av[offsetA * 2 + i * sa1 + (i + 1) * sa2];
            Av[offsetA * 2 + i * sa1 + (i + 1) * sa2] = 1;
            Av[offsetA * 2 + i * sa1 + (i + 1) * sa2 + 1] = 0;
            zgemv(
              "N",
              M - i - 1,
              N - i - 1,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zgemv(
              "C",
              N - i - 1,
              i + 1,
              ONE,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            zgemv(
              "N",
              M - i - 1,
              i + 1,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              X,
              strideX1,
              offsetX + i * strideX2,
              ONE,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zgemv(
              "N",
              i,
              N - i - 1,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + (i + 1) * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            zgemv(
              "N",
              M - i - 1,
              i,
              NEGONE,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              X,
              strideX1,
              offsetX + i * strideX2,
              ONE,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zscal(
              M - i - 1,
              TAUP.get(offsetTAUP + i * strideTAUP),
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
          }
        }
      } else {
        for (i = 0; i < nb; i++) {
          zlacgv(N - i, A, strideA2, offsetA + i * strideA1 + i * strideA2);
          zlacgv(i, A, strideA2, offsetA + i * strideA1);
          zgemv(
            "N",
            N - i,
            i,
            NEGONE,
            Y,
            strideY1,
            strideY2,
            offsetY + i * strideY1,
            A,
            strideA2,
            offsetA + i * strideA1,
            ONE,
            A,
            strideA2,
            offsetA + i * strideA1 + i * strideA2
          );
          zlacgv(i, A, strideA2, offsetA + i * strideA1);
          zlacgv(i, X, strideX2, offsetX + i * strideX1);
          zgemv(
            "C",
            i,
            N - i,
            NEGONE,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA2,
            X,
            strideX2,
            offsetX + i * strideX1,
            ONE,
            A,
            strideA2,
            offsetA + i * strideA1 + i * strideA2
          );
          zlacgv(i, X, strideX2, offsetX + i * strideX1);
          ia = offsetA * 2 + i * sa1 + i * sa2;
          alphaRe = Av[ia];
          alphaIm = Av[ia + 1];
          reinterpret2(alpha, 0)[0] = alphaRe;
          reinterpret2(alpha, 0)[1] = alphaIm;
          zlarfg(
            N - i,
            alpha,
            0,
            A,
            strideA2,
            offsetA + i * strideA1 + Math.min(i + 1, N - 1) * strideA2,
            TAUP,
            offsetTAUP + i * strideTAUP
          );
          d[offsetD + i * strideD] = reinterpret2(alpha, 0)[0];
          if (i < M - 1) {
            Av[ia] = 1;
            Av[ia + 1] = 0;
            zgemv(
              "N",
              M - i - 1,
              N - i,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zgemv(
              "C",
              N - i,
              i,
              ONE,
              Y,
              strideY1,
              strideY2,
              offsetY + i * strideY1,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            zgemv(
              "N",
              M - i - 1,
              i,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              X,
              strideX1,
              offsetX + i * strideX2,
              ONE,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zgemv(
              "N",
              i,
              N - i,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA2,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              ZERO,
              X,
              strideX1,
              offsetX + i * strideX2
            );
            zgemv(
              "N",
              M - i - 1,
              i,
              NEGONE,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              X,
              strideX1,
              offsetX + i * strideX2,
              ONE,
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zscal(
              M - i - 1,
              TAUP.get(offsetTAUP + i * strideTAUP),
              X,
              strideX1,
              offsetX + (i + 1) * strideX1 + i * strideX2
            );
            zlacgv(N - i, A, strideA2, offsetA + i * strideA1 + i * strideA2);
            zlacgv(i, Y, strideY2, offsetY + i * strideY1);
            zgemv(
              "N",
              M - i - 1,
              i,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              Y,
              strideY2,
              offsetY + i * strideY1,
              ONE,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2
            );
            zlacgv(i, Y, strideY2, offsetY + i * strideY1);
            zgemv(
              "N",
              M - i - 1,
              i + 1,
              NEGONE,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              A,
              strideA1,
              offsetA + i * strideA2,
              ONE,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2
            );
            zlarfg(
              M - i - 1,
              A,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              A,
              strideA1,
              offsetA + Math.min(i + 2, M - 1) * strideA1 + i * strideA2,
              TAUQ,
              offsetTAUQ + i * strideTAUQ
            );
            e[offsetE + i * strideE] = Av[offsetA * 2 + (i + 1) * sa1 + i * sa2];
            Av[offsetA * 2 + (i + 1) * sa1 + i * sa2] = 1;
            Av[offsetA * 2 + (i + 1) * sa1 + i * sa2 + 1] = 0;
            zgemv(
              "C",
              M - i - 1,
              N - i - 1,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + (i + 1) * strideA2,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zgemv(
              "C",
              M - i - 1,
              i,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            zgemv(
              "N",
              N - i - 1,
              i,
              NEGONE,
              Y,
              strideY1,
              strideY2,
              offsetY + (i + 1) * strideY1,
              Y,
              strideY1,
              offsetY + i * strideY2,
              ONE,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zgemv(
              "C",
              M - i - 1,
              i + 1,
              ONE,
              X,
              strideX1,
              strideX2,
              offsetX + (i + 1) * strideX1,
              A,
              strideA1,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              ZERO,
              Y,
              strideY1,
              offsetY + i * strideY2
            );
            zgemv(
              "C",
              i + 1,
              N - i - 1,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA2,
              Y,
              strideY1,
              offsetY + i * strideY2,
              ONE,
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
            zscal(
              N - i - 1,
              TAUQ.get(offsetTAUQ + i * strideTAUQ),
              Y,
              strideY1,
              offsetY + (i + 1) * strideY1 + i * strideY2
            );
          } else {
            zlacgv(N - i, A, strideA2, offsetA + i * strideA1 + i * strideA2);
          }
        }
      }
    }
    module.exports = zlabrd;
  }
});

// lib/lapack/base/zgebrd/lib/base.js
var require_base33 = __commonJS({
  "lib/lapack/base/zgebrd/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zgebd2 = require_base30();
    var zgemm = require_base31();
    var zlabrd = require_base32();
    var DEFAULT_NB = 32;
    var ONE = new Complex128(1, 0);
    var NEGONE = new Complex128(-1, 0);
    function zgebrd(M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK, lwork) {
      var ldwrkx;
      var ldwrky;
      var minmn;
      var nbmin;
      var sa1;
      var sa2;
      var oA;
      var Av;
      var aii;
      var aij;
      var nb;
      var nx;
      var ws;
      var i;
      var j;
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
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
        WORK = new Complex128Array2(ws);
        offsetWORK = 0;
        strideWORK = 1;
      }
      i = 0;
      if (nb >= 2 && nb < minmn && nx < minmn) {
        while (i < minmn - nx) {
          zlabrd(
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
            zgemm(
              "N",
              "C",
              M - i - nb,
              N - i - nb,
              nb,
              NEGONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + i * strideA2,
              WORK,
              1,
              ldwrky,
              offsetWORK + ldwrkx * nb + nb,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + (i + nb) * strideA2
            );
            zgemm(
              "N",
              "N",
              M - i - nb,
              N - i - nb,
              nb,
              NEGONE,
              WORK,
              1,
              ldwrkx,
              offsetWORK + nb,
              A,
              strideA1,
              strideA2,
              offsetA + i * strideA1 + (i + nb) * strideA2,
              ONE,
              A,
              strideA1,
              strideA2,
              offsetA + (i + nb) * strideA1 + (i + nb) * strideA2
            );
          }
          if (M >= N) {
            for (j = i; j < i + nb; j++) {
              aii = oA + j * sa1 + j * sa2;
              Av[aii] = d[offsetD + j * strideD];
              Av[aii + 1] = 0;
              aij = oA + j * sa1 + (j + 1) * sa2;
              Av[aij] = e[offsetE + j * strideE];
              Av[aij + 1] = 0;
            }
          } else {
            for (j = i; j < i + nb; j++) {
              aii = oA + j * sa1 + j * sa2;
              Av[aii] = d[offsetD + j * strideD];
              Av[aii + 1] = 0;
              aij = oA + (j + 1) * sa1 + j * sa2;
              Av[aij] = e[offsetE + j * strideE];
              Av[aij + 1] = 0;
            }
          }
          i += nb;
        }
      }
      zgebd2(
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
    module.exports = zgebrd;
  }
});

// lib/lapack/base/zgeqr2/lib/base.js
var require_base34 = __commonJS({
  "lib/lapack/base/zgeqr2/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var reinterpret2 = require_lib57();
    var zlarfg = require_base23();
    var zlarf = require_base28();
    function zgeqr2(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var conj_tau;
      var conj_f64;
      var alpha_re;
      var alpha_im;
      var tau_f64;
      var sa1;
      var sa2;
      var oA;
      var oT;
      var Av;
      var aii;
      var K;
      var i;
      Av = reinterpret2(A, 0);
      tau_f64 = reinterpret2(TAU, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      oT = offsetTAU * 2;
      K = Math.min(M, N);
      conj_tau = new Complex128Array2(1);
      conj_f64 = reinterpret2(conj_tau, 0);
      for (i = 0; i < K; i++) {
        aii = oA + i * sa1 + i * sa2;
        zlarfg(
          M - i,
          A,
          offsetA + i * strideA1 + i * strideA2,
          A,
          strideA1,
          offsetA + Math.min(i + 1, M - 1) * strideA1 + i * strideA2,
          TAU,
          offsetTAU + i * strideTAU
        );
        if (i < N - 1) {
          alpha_re = Av[aii];
          alpha_im = Av[aii + 1];
          Av[aii] = 1;
          Av[aii + 1] = 0;
          conj_f64[0] = tau_f64[oT + i * strideTAU * 2];
          conj_f64[1] = -tau_f64[oT + i * strideTAU * 2 + 1];
          zlarf(
            "L",
            M - i,
            N - i - 1,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2,
            conj_tau,
            0,
            A,
            strideA1,
            strideA2,
            offsetA + i * strideA1 + (i + 1) * strideA2,
            WORK,
            strideWORK,
            offsetWORK
          );
          Av[aii] = alpha_re;
          Av[aii + 1] = alpha_im;
        }
      }
      return 0;
    }
    module.exports = zgeqr2;
  }
});

// lib/blas/base/zcopy/lib/base.js
var require_base35 = __commonJS({
  "lib/blas/base/zcopy/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zcopy(N, zx, strideX, offsetX, zy, strideY, offsetY) {
      var xv;
      var yv;
      var sx;
      var sy;
      var ix;
      var iy;
      var i;
      if (N <= 0) {
        return zy;
      }
      xv = reinterpret2(zx, 0);
      yv = reinterpret2(zy, 0);
      ix = offsetX * 2;
      iy = offsetY * 2;
      sx = strideX * 2;
      sy = strideY * 2;
      for (i = 0; i < N; i++) {
        yv[iy] = xv[ix];
        yv[iy + 1] = xv[ix + 1];
        ix += sx;
        iy += sy;
      }
      return zy;
    }
    module.exports = zcopy;
  }
});

// lib/blas/base/ztrmm/lib/base.js
var require_base36 = __commonJS({
  "lib/blas/base/ztrmm/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function ztrmm(side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB) {
      var noconj;
      var nounit;
      var lside;
      var upper;
      var alphaR;
      var alphaI;
      var tempR;
      var tempI;
      var sa1;
      var sa2;
      var sb1;
      var sb2;
      var ia;
      var ib;
      var jb;
      var kb;
      var oA;
      var oB;
      var Av;
      var Bv;
      var ar;
      var ai;
      var br;
      var bi;
      var tr;
      var ti;
      var i;
      var j;
      var k;
      if (M === 0 || N === 0) {
        return B;
      }
      lside = side === "L" || side === "l";
      upper = uplo === "U" || uplo === "u";
      noconj = transa === "T" || transa === "t";
      nounit = diag === "N" || diag === "n";
      alphaR = real(alpha);
      alphaI = imag(alpha);
      Av = reinterpret2(A, 0);
      oA = offsetA * 2;
      Bv = reinterpret2(B, 0);
      oB = offsetB * 2;
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      sb1 = strideB1 * 2;
      sb2 = strideB2 * 2;
      if (alphaR === 0 && alphaI === 0) {
        for (j = 0; j < N; j++) {
          for (i = 0; i < M; i++) {
            ib = oB + i * sb1 + j * sb2;
            Bv[ib] = 0;
            Bv[ib + 1] = 0;
          }
        }
        return B;
      }
      if (lside) {
        if (transa === "N" || transa === "n") {
          if (upper) {
            for (j = 0; j < N; j++) {
              for (k = 0; k < M; k++) {
                kb = oB + k * sb1 + j * sb2;
                br = Bv[kb];
                bi = Bv[kb + 1];
                if (br !== 0 || bi !== 0) {
                  tempR = alphaR * br - alphaI * bi;
                  tempI = alphaR * bi + alphaI * br;
                  for (i = 0; i < k; i++) {
                    ib = oB + i * sb1 + j * sb2;
                    ia = oA + i * sa1 + k * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    Bv[ib] += tempR * ar - tempI * ai;
                    Bv[ib + 1] += tempR * ai + tempI * ar;
                  }
                  if (nounit) {
                    ia = oA + k * sa1 + k * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    Bv[kb] = tempR * ar - tempI * ai;
                    Bv[kb + 1] = tempR * ai + tempI * ar;
                  } else {
                    Bv[kb] = tempR;
                    Bv[kb + 1] = tempI;
                  }
                }
              }
            }
          } else {
            for (j = 0; j < N; j++) {
              for (k = M - 1; k >= 0; k--) {
                kb = oB + k * sb1 + j * sb2;
                br = Bv[kb];
                bi = Bv[kb + 1];
                if (br !== 0 || bi !== 0) {
                  tempR = alphaR * br - alphaI * bi;
                  tempI = alphaR * bi + alphaI * br;
                  Bv[kb] = tempR;
                  Bv[kb + 1] = tempI;
                  if (nounit) {
                    ia = oA + k * sa1 + k * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    Bv[kb] = tempR * ar - tempI * ai;
                    Bv[kb + 1] = tempR * ai + tempI * ar;
                  }
                  for (i = k + 1; i < M; i++) {
                    ib = oB + i * sb1 + j * sb2;
                    ia = oA + i * sa1 + k * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    Bv[ib] += tempR * ar - tempI * ai;
                    Bv[ib + 1] += tempR * ai + tempI * ar;
                  }
                }
              }
            }
          }
        } else {
          if (upper) {
            for (j = 0; j < N; j++) {
              for (i = M - 1; i >= 0; i--) {
                ib = oB + i * sb1 + j * sb2;
                tempR = Bv[ib];
                tempI = Bv[ib + 1];
                if (noconj) {
                  if (nounit) {
                    ia = oA + i * sa1 + i * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    tr = tempR * ar - tempI * ai;
                    ti = tempR * ai + tempI * ar;
                    tempR = tr;
                    tempI = ti;
                  }
                  for (k = 0; k < i; k++) {
                    ia = oA + k * sa1 + i * sa2;
                    kb = oB + k * sb1 + j * sb2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    tempR += ar * Bv[kb] - ai * Bv[kb + 1];
                    tempI += ar * Bv[kb + 1] + ai * Bv[kb];
                  }
                } else {
                  if (nounit) {
                    ia = oA + i * sa1 + i * sa2;
                    ar = Av[ia];
                    ai = -Av[ia + 1];
                    tr = tempR * ar - tempI * ai;
                    ti = tempR * ai + tempI * ar;
                    tempR = tr;
                    tempI = ti;
                  }
                  for (k = 0; k < i; k++) {
                    ia = oA + k * sa1 + i * sa2;
                    kb = oB + k * sb1 + j * sb2;
                    ar = Av[ia];
                    ai = -Av[ia + 1];
                    tempR += ar * Bv[kb] - ai * Bv[kb + 1];
                    tempI += ar * Bv[kb + 1] + ai * Bv[kb];
                  }
                }
                Bv[ib] = alphaR * tempR - alphaI * tempI;
                Bv[ib + 1] = alphaR * tempI + alphaI * tempR;
              }
            }
          } else {
            for (j = 0; j < N; j++) {
              for (i = 0; i < M; i++) {
                ib = oB + i * sb1 + j * sb2;
                tempR = Bv[ib];
                tempI = Bv[ib + 1];
                if (noconj) {
                  if (nounit) {
                    ia = oA + i * sa1 + i * sa2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    tr = tempR * ar - tempI * ai;
                    ti = tempR * ai + tempI * ar;
                    tempR = tr;
                    tempI = ti;
                  }
                  for (k = i + 1; k < M; k++) {
                    ia = oA + k * sa1 + i * sa2;
                    kb = oB + k * sb1 + j * sb2;
                    ar = Av[ia];
                    ai = Av[ia + 1];
                    tempR += ar * Bv[kb] - ai * Bv[kb + 1];
                    tempI += ar * Bv[kb + 1] + ai * Bv[kb];
                  }
                } else {
                  if (nounit) {
                    ia = oA + i * sa1 + i * sa2;
                    ar = Av[ia];
                    ai = -Av[ia + 1];
                    tr = tempR * ar - tempI * ai;
                    ti = tempR * ai + tempI * ar;
                    tempR = tr;
                    tempI = ti;
                  }
                  for (k = i + 1; k < M; k++) {
                    ia = oA + k * sa1 + i * sa2;
                    kb = oB + k * sb1 + j * sb2;
                    ar = Av[ia];
                    ai = -Av[ia + 1];
                    tempR += ar * Bv[kb] - ai * Bv[kb + 1];
                    tempI += ar * Bv[kb + 1] + ai * Bv[kb];
                  }
                }
                Bv[ib] = alphaR * tempR - alphaI * tempI;
                Bv[ib + 1] = alphaR * tempI + alphaI * tempR;
              }
            }
          }
        }
      } else {
        if (transa === "N" || transa === "n") {
          if (upper) {
            for (j = N - 1; j >= 0; j--) {
              tempR = alphaR;
              tempI = alphaI;
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr = tempR * ar - tempI * ai;
                ti = tempR * ai + tempI * ar;
                tempR = tr;
                tempI = ti;
              }
              for (i = 0; i < M; i++) {
                ib = oB + i * sb1 + j * sb2;
                br = Bv[ib];
                bi = Bv[ib + 1];
                Bv[ib] = tempR * br - tempI * bi;
                Bv[ib + 1] = tempR * bi + tempI * br;
              }
              for (k = 0; k < j; k++) {
                ia = oA + k * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (ar !== 0 || ai !== 0) {
                  tr = alphaR * ar - alphaI * ai;
                  ti = alphaR * ai + alphaI * ar;
                  for (i = 0; i < M; i++) {
                    ib = oB + i * sb1 + j * sb2;
                    kb = oB + i * sb1 + k * sb2;
                    Bv[ib] += tr * Bv[kb] - ti * Bv[kb + 1];
                    Bv[ib + 1] += tr * Bv[kb + 1] + ti * Bv[kb];
                  }
                }
              }
            }
          } else {
            for (j = 0; j < N; j++) {
              tempR = alphaR;
              tempI = alphaI;
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr = tempR * ar - tempI * ai;
                ti = tempR * ai + tempI * ar;
                tempR = tr;
                tempI = ti;
              }
              for (i = 0; i < M; i++) {
                ib = oB + i * sb1 + j * sb2;
                br = Bv[ib];
                bi = Bv[ib + 1];
                Bv[ib] = tempR * br - tempI * bi;
                Bv[ib + 1] = tempR * bi + tempI * br;
              }
              for (k = j + 1; k < N; k++) {
                ia = oA + k * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (ar !== 0 || ai !== 0) {
                  tr = alphaR * ar - alphaI * ai;
                  ti = alphaR * ai + alphaI * ar;
                  for (i = 0; i < M; i++) {
                    ib = oB + i * sb1 + j * sb2;
                    kb = oB + i * sb1 + k * sb2;
                    Bv[ib] += tr * Bv[kb] - ti * Bv[kb + 1];
                    Bv[ib + 1] += tr * Bv[kb + 1] + ti * Bv[kb];
                  }
                }
              }
            }
          }
        } else {
          if (upper) {
            for (k = 0; k < N; k++) {
              for (j = 0; j < k; j++) {
                ia = oA + j * sa1 + k * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (ar !== 0 || ai !== 0) {
                  if (!noconj) {
                    ai = -ai;
                  }
                  tr = alphaR * ar - alphaI * ai;
                  ti = alphaR * ai + alphaI * ar;
                  for (i = 0; i < M; i++) {
                    jb = oB + i * sb1 + j * sb2;
                    kb = oB + i * sb1 + k * sb2;
                    Bv[jb] += tr * Bv[kb] - ti * Bv[kb + 1];
                    Bv[jb + 1] += tr * Bv[kb + 1] + ti * Bv[kb];
                  }
                }
              }
              tempR = alphaR;
              tempI = alphaI;
              if (nounit) {
                ia = oA + k * sa1 + k * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (!noconj) {
                  ai = -ai;
                }
                tr = tempR * ar - tempI * ai;
                ti = tempR * ai + tempI * ar;
                tempR = tr;
                tempI = ti;
              }
              if (tempR !== 1 || tempI !== 0) {
                for (i = 0; i < M; i++) {
                  kb = oB + i * sb1 + k * sb2;
                  br = Bv[kb];
                  bi = Bv[kb + 1];
                  Bv[kb] = tempR * br - tempI * bi;
                  Bv[kb + 1] = tempR * bi + tempI * br;
                }
              }
            }
          } else {
            for (k = N - 1; k >= 0; k--) {
              for (j = k + 1; j < N; j++) {
                ia = oA + j * sa1 + k * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (ar !== 0 || ai !== 0) {
                  if (!noconj) {
                    ai = -ai;
                  }
                  tr = alphaR * ar - alphaI * ai;
                  ti = alphaR * ai + alphaI * ar;
                  for (i = 0; i < M; i++) {
                    jb = oB + i * sb1 + j * sb2;
                    kb = oB + i * sb1 + k * sb2;
                    Bv[jb] += tr * Bv[kb] - ti * Bv[kb + 1];
                    Bv[jb + 1] += tr * Bv[kb + 1] + ti * Bv[kb];
                  }
                }
              }
              tempR = alphaR;
              tempI = alphaI;
              if (nounit) {
                ia = oA + k * sa1 + k * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                if (!noconj) {
                  ai = -ai;
                }
                tr = tempR * ar - tempI * ai;
                ti = tempR * ai + tempI * ar;
                tempR = tr;
                tempI = ti;
              }
              if (tempR !== 1 || tempI !== 0) {
                for (i = 0; i < M; i++) {
                  kb = oB + i * sb1 + k * sb2;
                  br = Bv[kb];
                  bi = Bv[kb + 1];
                  Bv[kb] = tempR * br - tempI * bi;
                  Bv[kb + 1] = tempR * bi + tempI * br;
                }
              }
            }
          }
        }
      }
      return B;
    }
    module.exports = ztrmm;
  }
});

// lib/lapack/base/zlarfb/lib/base.js
var require_base37 = __commonJS({
  "lib/lapack/base/zlarfb/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zcopy = require_base35();
    var zgemm = require_base31();
    var ztrmm = require_base36();
    var zlacgv = require_base29();
    var ONE = new Complex128(1, 0);
    var NEGONE = new Complex128(-1, 0);
    function zlarfb(side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK) {
      var transt;
      var Cv;
      var Wv;
      var sw1;
      var sw2;
      var sc1;
      var sc2;
      var oW;
      var oC;
      var iw;
      var ic;
      var i;
      var j;
      if (M <= 0 || N <= 0) {
        return;
      }
      Cv = reinterpret2(C, 0);
      Wv = reinterpret2(WORK, 0);
      sw1 = strideWORK1 * 2;
      sw2 = strideWORK2 * 2;
      sc1 = strideC1 * 2;
      sc2 = strideC2 * 2;
      oW = offsetWORK * 2;
      oC = offsetC * 2;
      if (trans === "N" || trans === "n") {
        transt = "C";
      } else {
        transt = "N";
      }
      if (storev === "C" || storev === "c") {
        if (direct === "F" || direct === "f") {
          if (side === "L" || side === "l") {
            for (j = 0; j < K; j++) {
              zcopy(N, C, strideC2, offsetC + j * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
              zlacgv(N, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "L", "N", "U", N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "N",
                N,
                K,
                M - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "U", transt, "N", N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "N",
                "C",
                M - K,
                N,
                K,
                NEGONE,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1
              );
            }
            ztrmm("R", "L", "C", "U", N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < N; i++) {
                ic = oC + j * sc1 + i * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= -Wv[iw + 1];
              }
            }
          } else if (side === "R" || side === "r") {
            for (j = 0; j < K; j++) {
              zcopy(M, C, strideC1, offsetC + j * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "L", "N", "U", M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "N",
                M,
                K,
                N - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "U", trans, "N", M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "C",
                M,
                N - K,
                K,
                NEGONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV1,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2
              );
            }
            ztrmm("R", "L", "C", "U", M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < M; i++) {
                ic = oC + i * sc1 + j * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= Wv[iw + 1];
              }
            }
          }
        } else {
          if (side === "L" || side === "l") {
            for (j = 0; j < K; j++) {
              zcopy(N, C, strideC2, offsetC + (M - K + j) * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
              zlacgv(N, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "U", "N", "U", N, K, ONE, V, strideV1, strideV2, offsetV + (M - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "N",
                N,
                K,
                M - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "L", transt, "N", N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "N",
                "C",
                M - K,
                N,
                K,
                NEGONE,
                V,
                strideV1,
                strideV2,
                offsetV,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC
              );
            }
            ztrmm("R", "U", "C", "U", N, K, ONE, V, strideV1, strideV2, offsetV + (M - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < N; i++) {
                ic = oC + (M - K + j) * sc1 + i * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= -Wv[iw + 1];
              }
            }
          } else if (side === "R" || side === "r") {
            for (j = 0; j < K; j++) {
              zcopy(M, C, strideC1, offsetC + (N - K + j) * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "U", "N", "U", M, K, ONE, V, strideV1, strideV2, offsetV + (N - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "N",
                M,
                K,
                N - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "L", trans, "N", M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "C",
                M,
                N - K,
                K,
                NEGONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC
              );
            }
            ztrmm("R", "U", "C", "U", M, K, ONE, V, strideV1, strideV2, offsetV + (N - K) * strideV1, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < M; i++) {
                ic = oC + i * sc1 + (N - K + j) * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= Wv[iw + 1];
              }
            }
          }
        }
      } else {
        if (direct === "F" || direct === "f") {
          if (side === "L" || side === "l") {
            for (j = 0; j < K; j++) {
              zcopy(N, C, strideC2, offsetC + j * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
              zlacgv(N, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "U", "C", "U", N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "C",
                N,
                K,
                M - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV2,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "U", transt, "N", N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "C",
                M - K,
                N,
                K,
                NEGONE,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV2,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC1
              );
            }
            ztrmm("R", "U", "N", "U", N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < N; i++) {
                ic = oC + j * sc1 + i * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= -Wv[iw + 1];
              }
            }
          } else if (side === "R" || side === "r") {
            for (j = 0; j < K; j++) {
              zcopy(M, C, strideC1, offsetC + j * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "U", "C", "U", M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "C",
                M,
                K,
                N - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV2,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "U", trans, "N", M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "N",
                M,
                N - K,
                K,
                NEGONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                V,
                strideV1,
                strideV2,
                offsetV + K * strideV2,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC + K * strideC2
              );
            }
            ztrmm("R", "U", "N", "U", M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < M; i++) {
                ic = oC + i * sc1 + j * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= Wv[iw + 1];
              }
            }
          }
        } else {
          if (side === "L" || side === "l") {
            for (j = 0; j < K; j++) {
              zcopy(N, C, strideC2, offsetC + (M - K + j) * strideC1, WORK, strideWORK1, offsetWORK + j * strideWORK2);
              zlacgv(N, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "L", "C", "U", N, K, ONE, V, strideV1, strideV2, offsetV + (M - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "C",
                N,
                K,
                M - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "L", transt, "N", N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (M > K) {
              zgemm(
                "C",
                "C",
                M - K,
                N,
                K,
                NEGONE,
                V,
                strideV1,
                strideV2,
                offsetV,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC
              );
            }
            ztrmm("R", "L", "N", "U", N, K, ONE, V, strideV1, strideV2, offsetV + (M - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < N; i++) {
                ic = oC + (M - K + j) * sc1 + i * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= -Wv[iw + 1];
              }
            }
          } else if (side === "R" || side === "r") {
            for (j = 0; j < K; j++) {
              zcopy(M, C, strideC1, offsetC + (N - K + j) * strideC2, WORK, strideWORK1, offsetWORK + j * strideWORK2);
            }
            ztrmm("R", "L", "C", "U", M, K, ONE, V, strideV1, strideV2, offsetV + (N - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "C",
                M,
                K,
                N - K,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK
              );
            }
            ztrmm("R", "L", trans, "N", M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK);
            if (N > K) {
              zgemm(
                "N",
                "N",
                M,
                N - K,
                K,
                NEGONE,
                WORK,
                strideWORK1,
                strideWORK2,
                offsetWORK,
                V,
                strideV1,
                strideV2,
                offsetV,
                ONE,
                C,
                strideC1,
                strideC2,
                offsetC
              );
            }
            ztrmm("R", "L", "N", "U", M, K, ONE, V, strideV1, strideV2, offsetV + (N - K) * strideV2, WORK, strideWORK1, strideWORK2, offsetWORK);
            for (j = 0; j < K; j++) {
              for (i = 0; i < M; i++) {
                ic = oC + i * sc1 + (N - K + j) * sc2;
                iw = oW + i * sw1 + j * sw2;
                Cv[ic] -= Wv[iw];
                Cv[ic + 1] -= Wv[iw + 1];
              }
            }
          }
        }
      }
    }
    module.exports = zlarfb;
  }
});

// lib/blas/base/ztrmv/lib/base.js
var require_base38 = __commonJS({
  "lib/blas/base/ztrmv/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function ztrmv(uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX) {
      var noconj;
      var nounit;
      var upper;
      var sa1;
      var sa2;
      var sx;
      var ix;
      var jx;
      var ia;
      var oA;
      var oX;
      var Av;
      var xv;
      var tr;
      var ti;
      var ar;
      var ai;
      var xr;
      var xi;
      var i;
      var j;
      if (N <= 0) {
        return x;
      }
      upper = uplo === "U" || uplo === "u";
      noconj = trans === "T" || trans === "t";
      nounit = diag === "N" || diag === "n";
      Av = reinterpret2(A, 0);
      oA = offsetA * 2;
      xv = reinterpret2(x, 0);
      oX = offsetX * 2;
      sx = strideX * 2;
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      if (trans === "N" || trans === "n") {
        if (upper) {
          jx = oX;
          for (j = 0; j < N; j++) {
            xr = xv[jx];
            xi = xv[jx + 1];
            if (xr !== 0 || xi !== 0) {
              ix = oX;
              for (i = 0; i < j; i++) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                xv[ix] += xr * ar - xi * ai;
                xv[ix + 1] += xr * ai + xi * ar;
                ix += sx;
              }
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr = xr * ar - xi * ai;
                ti = xr * ai + xi * ar;
                xv[jx] = tr;
                xv[jx + 1] = ti;
              }
            }
            jx += sx;
          }
        } else {
          jx = oX + (N - 1) * sx;
          for (j = N - 1; j >= 0; j--) {
            xr = xv[jx];
            xi = xv[jx + 1];
            if (xr !== 0 || xi !== 0) {
              ix = oX + (N - 1) * sx;
              for (i = N - 1; i > j; i--) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                xv[ix] += xr * ar - xi * ai;
                xv[ix + 1] += xr * ai + xi * ar;
                ix -= sx;
              }
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr = xr * ar - xi * ai;
                ti = xr * ai + xi * ar;
                xv[jx] = tr;
                xv[jx + 1] = ti;
              }
            }
            jx -= sx;
          }
        }
      } else {
        if (upper) {
          jx = oX + (N - 1) * sx;
          for (j = N - 1; j >= 0; j--) {
            tr = xv[jx];
            ti = xv[jx + 1];
            if (noconj) {
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                xr = tr * ar - ti * ai;
                xi = tr * ai + ti * ar;
                tr = xr;
                ti = xi;
              }
              ix = oX + (j - 1) * sx;
              for (i = j - 1; i >= 0; i--) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr += ar * xv[ix] - ai * xv[ix + 1];
                ti += ar * xv[ix + 1] + ai * xv[ix];
                ix -= sx;
              }
            } else {
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = -Av[ia + 1];
                xr = tr * ar - ti * ai;
                xi = tr * ai + ti * ar;
                tr = xr;
                ti = xi;
              }
              ix = oX + (j - 1) * sx;
              for (i = j - 1; i >= 0; i--) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = -Av[ia + 1];
                tr += ar * xv[ix] - ai * xv[ix + 1];
                ti += ar * xv[ix + 1] + ai * xv[ix];
                ix -= sx;
              }
            }
            xv[jx] = tr;
            xv[jx + 1] = ti;
            jx -= sx;
          }
        } else {
          jx = oX;
          for (j = 0; j < N; j++) {
            tr = xv[jx];
            ti = xv[jx + 1];
            if (noconj) {
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                xr = tr * ar - ti * ai;
                xi = tr * ai + ti * ar;
                tr = xr;
                ti = xi;
              }
              ix = oX + (j + 1) * sx;
              for (i = j + 1; i < N; i++) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = Av[ia + 1];
                tr += ar * xv[ix] - ai * xv[ix + 1];
                ti += ar * xv[ix + 1] + ai * xv[ix];
                ix += sx;
              }
            } else {
              if (nounit) {
                ia = oA + j * sa1 + j * sa2;
                ar = Av[ia];
                ai = -Av[ia + 1];
                xr = tr * ar - ti * ai;
                xi = tr * ai + ti * ar;
                tr = xr;
                ti = xi;
              }
              ix = oX + (j + 1) * sx;
              for (i = j + 1; i < N; i++) {
                ia = oA + i * sa1 + j * sa2;
                ar = Av[ia];
                ai = -Av[ia + 1];
                tr += ar * xv[ix] - ai * xv[ix + 1];
                ti += ar * xv[ix + 1] + ai * xv[ix];
                ix += sx;
              }
            }
            xv[jx] = tr;
            xv[jx + 1] = ti;
            jx += sx;
          }
        }
      }
      return x;
    }
    module.exports = ztrmv;
  }
});

// lib/lapack/base/zlarft/lib/base.js
var require_base39 = __commonJS({
  "lib/lapack/base/zlarft/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zgemv = require_base24();
    var zgemm = require_base31();
    var ztrmv = require_base38();
    var ONE = new Complex128(1, 0);
    function zlarft(direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT) {
      var prevlastv;
      var negTauI;
      var negTauR;
      var negTau;
      var lastv;
      var tauR;
      var tauI;
      var TAUv;
      var Vv;
      var Tv;
      var vr;
      var vi;
      var sv1;
      var sv2;
      var st1;
      var st2;
      var stau;
      var oV;
      var oTAU;
      var oT;
      var it;
      var iv;
      var i;
      var j;
      var jj;
      if (N === 0) {
        return;
      }
      Vv = reinterpret2(V, 0);
      TAUv = reinterpret2(TAU, 0);
      Tv = reinterpret2(T, 0);
      sv1 = strideV1 * 2;
      sv2 = strideV2 * 2;
      st1 = strideT1 * 2;
      st2 = strideT2 * 2;
      stau = strideTAU * 2;
      oV = offsetV * 2;
      oTAU = offsetTAU * 2;
      oT = offsetT * 2;
      if (direct === "F" || direct === "f") {
        prevlastv = N;
        for (i = 0; i < K; i++) {
          prevlastv = Math.max(prevlastv, i);
          tauR = TAUv[oTAU + i * stau];
          tauI = TAUv[oTAU + i * stau + 1];
          if (tauR === 0 && tauI === 0) {
            for (j = 0; j <= i; j++) {
              it = oT + j * st1 + i * st2;
              Tv[it] = 0;
              Tv[it + 1] = 0;
            }
          } else {
            if (storev === "C" || storev === "c") {
              lastv = N;
              for (jj = N - 1; jj > i; jj--) {
                iv = oV + jj * sv1 + i * sv2;
                if (Vv[iv] !== 0 || Vv[iv + 1] !== 0) {
                  break;
                }
                lastv = jj;
              }
              negTauR = -tauR;
              negTauI = -tauI;
              for (j = 0; j < i; j++) {
                iv = oV + i * sv1 + j * sv2;
                vr = Vv[iv];
                vi = -Vv[iv + 1];
                it = oT + j * st1 + i * st2;
                Tv[it] = negTauR * vr - negTauI * vi;
                Tv[it + 1] = negTauR * vi + negTauI * vr;
              }
              jj = Math.min(lastv, prevlastv);
              if (jj - i - 1 > 0) {
                negTau = new Complex128(negTauR, negTauI);
                zgemv(
                  "C",
                  jj - i - 1,
                  i,
                  negTau,
                  V,
                  strideV1,
                  strideV2,
                  offsetV + (i + 1) * strideV1,
                  V,
                  strideV1,
                  offsetV + (i + 1) * strideV1 + i * strideV2,
                  ONE,
                  T,
                  strideT1,
                  offsetT + i * strideT2
                );
              }
            } else {
              lastv = N;
              for (jj = N - 1; jj > i; jj--) {
                iv = oV + i * sv1 + jj * sv2;
                if (Vv[iv] !== 0 || Vv[iv + 1] !== 0) {
                  break;
                }
                lastv = jj;
              }
              negTauR = -tauR;
              negTauI = -tauI;
              for (j = 0; j < i; j++) {
                iv = oV + j * sv1 + i * sv2;
                vr = Vv[iv];
                vi = Vv[iv + 1];
                it = oT + j * st1 + i * st2;
                Tv[it] = negTauR * vr - negTauI * vi;
                Tv[it + 1] = negTauR * vi + negTauI * vr;
              }
              jj = Math.min(lastv, prevlastv);
              if (jj - i - 1 > 0) {
                var negTauR2 = new Complex128(negTauR, negTauI);
                zgemm(
                  "N",
                  "C",
                  i,
                  1,
                  jj - i - 1,
                  negTauR2,
                  V,
                  strideV1,
                  strideV2,
                  offsetV + (i + 1) * strideV2,
                  V,
                  strideV1,
                  strideV2,
                  offsetV + i * strideV1 + (i + 1) * strideV2,
                  ONE,
                  T,
                  strideT1,
                  strideT2,
                  offsetT + i * strideT2
                );
              }
            }
            if (i > 0) {
              ztrmv(
                "U",
                "N",
                "N",
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
            it = oT + i * st1 + i * st2;
            Tv[it] = tauR;
            Tv[it + 1] = tauI;
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
          tauR = TAUv[oTAU + i * stau];
          tauI = TAUv[oTAU + i * stau + 1];
          if (tauR === 0 && tauI === 0) {
            for (j = i; j < K; j++) {
              it = oT + j * st1 + i * st2;
              Tv[it] = 0;
              Tv[it + 1] = 0;
            }
          } else {
            if (i < K - 1) {
              if (storev === "C" || storev === "c") {
                lastv = 0;
                for (jj = 0; jj < i; jj++) {
                  iv = oV + jj * sv1 + i * sv2;
                  if (Vv[iv] !== 0 || Vv[iv + 1] !== 0) {
                    break;
                  }
                  lastv = jj + 1;
                }
                negTauR = -tauR;
                negTauI = -tauI;
                for (j = i + 1; j < K; j++) {
                  iv = oV + (N - K + i) * sv1 + j * sv2;
                  vr = Vv[iv];
                  vi = -Vv[iv + 1];
                  it = oT + j * st1 + i * st2;
                  Tv[it] = negTauR * vr - negTauI * vi;
                  Tv[it + 1] = negTauR * vi + negTauI * vr;
                }
                jj = Math.max(lastv, prevlastv);
                if (N - K + i - jj > 0) {
                  var negTauB = new Complex128(negTauR, negTauI);
                  zgemv(
                    "C",
                    N - K + i - jj,
                    K - i - 1,
                    negTauB,
                    V,
                    strideV1,
                    strideV2,
                    offsetV + jj * strideV1 + (i + 1) * strideV2,
                    V,
                    strideV1,
                    offsetV + jj * strideV1 + i * strideV2,
                    ONE,
                    T,
                    strideT1,
                    offsetT + (i + 1) * strideT1 + i * strideT2
                  );
                }
              } else {
                lastv = 0;
                for (jj = 0; jj < i; jj++) {
                  iv = oV + i * sv1 + jj * sv2;
                  if (Vv[iv] !== 0 || Vv[iv + 1] !== 0) {
                    break;
                  }
                  lastv = jj + 1;
                }
                negTauR = -tauR;
                negTauI = -tauI;
                for (j = i + 1; j < K; j++) {
                  iv = oV + j * sv1 + (N - K + i) * sv2;
                  vr = Vv[iv];
                  vi = Vv[iv + 1];
                  it = oT + j * st1 + i * st2;
                  Tv[it] = negTauR * vr - negTauI * vi;
                  Tv[it + 1] = negTauR * vi + negTauI * vr;
                }
                jj = Math.max(lastv, prevlastv);
                if (N - K + i - jj > 0) {
                  var negTauB2 = new Complex128(negTauR, negTauI);
                  zgemm(
                    "N",
                    "C",
                    K - i - 1,
                    1,
                    N - K + i - jj,
                    negTauB2,
                    V,
                    strideV1,
                    strideV2,
                    offsetV + (i + 1) * strideV1 + jj * strideV2,
                    V,
                    strideV1,
                    strideV2,
                    offsetV + i * strideV1 + jj * strideV2,
                    ONE,
                    T,
                    strideT1,
                    strideT2,
                    offsetT + (i + 1) * strideT1 + i * strideT2
                  );
                }
              }
              ztrmv(
                "L",
                "N",
                "N",
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
            it = oT + i * st1 + i * st2;
            Tv[it] = tauR;
            Tv[it + 1] = tauI;
          }
        }
      }
    }
    module.exports = zlarft;
  }
});

// lib/lapack/base/zgeqrf/lib/base.js
var require_base40 = __commonJS({
  "lib/lapack/base/zgeqrf/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var zgeqr2 = require_base34();
    var zlarfb = require_base37();
    var zlarft = require_base39();
    var DEFAULT_NB = 32;
    function zgeqrf(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
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
      T = new Complex128Array2(nb * nb);
      if (!WORK || WORK.length < iws) {
        WORK = new Complex128Array2(iws);
        offsetWORK = 0;
        strideWORK = 1;
      }
      ldwork = N;
      if (nb >= nbmin && nb < K && nx < K) {
        i = 0;
        while (i <= K - 1 - nx) {
          ib = Math.min(K - i, nb);
          zgeqr2(
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
            zlarft(
              "F",
              "C",
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
            zlarfb(
              "L",
              "C",
              "F",
              "C",
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
        zgeqr2(
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
    module.exports = zgeqrf;
  }
});

// lib/lapack/base/zlacpy/lib/base.js
var require_base41 = __commonJS({
  "lib/lapack/base/zlacpy/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    function zlacpy(uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB) {
      var Av;
      var Bv;
      var sa1;
      var sa2;
      var sb1;
      var sb2;
      var oA;
      var oB;
      var ia;
      var ib;
      var i;
      var j;
      Av = reinterpret2(A, 0);
      Bv = reinterpret2(B, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      sb1 = strideB1 * 2;
      sb2 = strideB2 * 2;
      oA = offsetA * 2;
      oB = offsetB * 2;
      if (uplo === "U" || uplo === "u") {
        for (j = 0; j < N; j++) {
          ia = oA + j * sa2;
          ib = oB + j * sb2;
          for (i = 0; i <= j && i < M; i++) {
            Bv[ib + i * sb1] = Av[ia + i * sa1];
            Bv[ib + i * sb1 + 1] = Av[ia + i * sa1 + 1];
          }
        }
      } else if (uplo === "L" || uplo === "l") {
        for (j = 0; j < N; j++) {
          ia = oA + j * sa2;
          ib = oB + j * sb2;
          for (i = j; i < M; i++) {
            Bv[ib + i * sb1] = Av[ia + i * sa1];
            Bv[ib + i * sb1 + 1] = Av[ia + i * sa1 + 1];
          }
        }
      } else {
        for (j = 0; j < N; j++) {
          ia = oA + j * sa2;
          ib = oB + j * sb2;
          for (i = 0; i < M; i++) {
            Bv[ib + i * sb1] = Av[ia + i * sa1];
            Bv[ib + i * sb1 + 1] = Av[ia + i * sa1 + 1];
          }
        }
      }
      return B;
    }
    module.exports = zlacpy;
  }
});

// lib/lapack/base/zlassq/lib/base.js
var require_base42 = __commonJS({
  "lib/lapack/base/zlassq/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var TSML = Math.pow(2, -511);
    var TBIG = Math.pow(2, 486);
    var SSML = Math.pow(2, 537);
    var SBIG = Math.pow(2, -538);
    function zlassq(N, x, stride, offset, scale, sumsq) {
      var notbig;
      var abig;
      var amed;
      var asml;
      var ymax;
      var ymin;
      var ax;
      var xv;
      var sx;
      var ix;
      var i;
      if (scale !== scale || sumsq !== sumsq) {
        return { scl: scale, sumsq };
      }
      if (sumsq === 0) {
        scale = 1;
      }
      if (scale === 0) {
        scale = 1;
        sumsq = 0;
      }
      if (N <= 0) {
        return { scl: scale, sumsq };
      }
      xv = reinterpret2(x, 0);
      notbig = true;
      asml = 0;
      amed = 0;
      abig = 0;
      sx = stride * 2;
      ix = offset * 2;
      if (stride < 0) {
        ix = offset * 2 - (N - 1) * sx;
      }
      for (i = 0; i < N; i++) {
        ax = Math.abs(xv[ix]);
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
        ax = Math.abs(xv[ix + 1]);
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
        ix += sx;
      }
      if (sumsq > 0) {
        ax = scale * Math.sqrt(sumsq);
        if (ax > TBIG) {
          if (scale > 1) {
            scale = scale * SBIG;
            abig += scale * (scale * sumsq);
          } else {
            abig += scale * (scale * (SBIG * (SBIG * sumsq)));
          }
        } else if (ax < TSML) {
          if (notbig) {
            if (scale < 1) {
              scale = scale * SSML;
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
      return { scl: scale, sumsq };
    }
    module.exports = zlassq;
  }
});

// lib/lapack/base/zlange/lib/base.js
var require_base43 = __commonJS({
  "lib/lapack/base/zlange/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var zlassq = require_base42();
    var cmplx = require_cmplx();
    function zlange(norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK) {
      var value;
      var scale;
      var temp;
      var sum;
      var out;
      var Av;
      var sa1;
      var sa2;
      var oA;
      var ai;
      var wi;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return 0;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      if (norm === "M" || norm === "m") {
        value = 0;
        for (j = 0; j < N; j++) {
          ai = oA + j * sa2;
          for (i = 0; i < M; i++) {
            temp = cmplx.absAt(Av, ai);
            if (value < temp || temp !== temp) {
              value = temp;
            }
            ai += sa1;
          }
        }
      } else if (norm === "O" || norm === "o" || norm === "1") {
        value = 0;
        for (j = 0; j < N; j++) {
          sum = 0;
          ai = oA + j * sa2;
          for (i = 0; i < M; i++) {
            sum += cmplx.absAt(Av, ai);
            ai += sa1;
          }
          if (value < sum || sum !== sum) {
            value = sum;
          }
        }
      } else if (norm === "I" || norm === "i") {
        for (i = 0; i < M; i++) {
          wi = offsetWORK + i * strideWORK;
          WORK[wi] = 0;
        }
        for (j = 0; j < N; j++) {
          ai = oA + j * sa2;
          wi = offsetWORK;
          for (i = 0; i < M; i++) {
            WORK[wi] += cmplx.absAt(Av, ai);
            ai += sa1;
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
      } else if (norm === "F" || norm === "f" || norm === "E" || norm === "e") {
        scale = 0;
        sum = 1;
        for (j = 0; j < N; j++) {
          out = zlassq(M, A, strideA1, offsetA + j * strideA2, scale, sum);
          scale = out.scl;
          sum = out.sumsq;
        }
        value = scale * Math.sqrt(sum);
      } else {
        value = 0;
      }
      return value;
    }
    module.exports = zlange;
  }
});

// lib/lapack/base/zlascl/lib/base.js
var require_base44 = __commonJS({
  "lib/lapack/base/zlascl/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var dlamch = require_base();
    function zlascl(type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA) {
      var smlnum;
      var bignum;
      var cfromc;
      var cfrom1;
      var ctoc;
      var cto1;
      var done;
      var itype;
      var mul;
      var Av;
      var sa1;
      var sa2;
      var oA;
      var ai;
      var k1;
      var k2;
      var k3;
      var k4;
      var iMax;
      var iMin;
      var i;
      var j;
      var c;
      c = type.charAt(0).toUpperCase();
      if (c === "G") {
        itype = 0;
      } else if (c === "L") {
        itype = 1;
      } else if (c === "U") {
        itype = 2;
      } else if (c === "H") {
        itype = 3;
      } else if (c === "B") {
        itype = 4;
      } else if (c === "Q") {
        itype = 5;
      } else if (c === "Z") {
        itype = 6;
      } else {
        return -1;
      }
      if (N === 0 || M === 0) {
        return 0;
      }
      smlnum = dlamch("S");
      bignum = 1 / smlnum;
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
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
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        } else if (itype === 1) {
          for (j = 0; j < N; j++) {
            for (i = j; i < M; i++) {
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        } else if (itype === 2) {
          for (j = 0; j < N; j++) {
            iMax = Math.min(j + 1, M);
            for (i = 0; i < iMax; i++) {
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        } else if (itype === 3) {
          for (j = 0; j < N; j++) {
            iMax = Math.min(j + 2, M);
            for (i = 0; i < iMax; i++) {
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        } else if (itype === 4) {
          k3 = kl + 1;
          k4 = N + 1;
          for (j = 0; j < N; j++) {
            iMax = Math.min(k3, k4 - j - 1);
            for (i = 0; i < iMax; i++) {
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        } else if (itype === 5) {
          k1 = ku + 2;
          k3 = ku + 1;
          for (j = 0; j < N; j++) {
            iMin = Math.max(k1 - j - 2, 0);
            for (i = iMin; i < k3; i++) {
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
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
              ai = oA + i * sa1 + j * sa2;
              Av[ai] *= mul;
              Av[ai + 1] *= mul;
            }
          }
        }
      }
      return 0;
    }
    module.exports = zlascl;
  }
});

// lib/lapack/base/zlaset/lib/base.js
var require_base45 = __commonJS({
  "lib/lapack/base/zlaset/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var real = require_lib54();
    var imag = require_lib55();
    function zlaset(uplo, M, N, alpha, beta, A, strideA1, strideA2, offsetA) {
      var alphaRe;
      var alphaIm;
      var betaRe;
      var betaIm;
      var Av;
      var sa1;
      var sa2;
      var oA;
      var mn;
      var idx;
      var i;
      var j;
      alphaRe = real(alpha);
      alphaIm = imag(alpha);
      betaRe = real(beta);
      betaIm = imag(beta);
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      mn = Math.min(M, N);
      if (uplo === "U" || uplo === "u") {
        for (j = 1; j < N; j++) {
          idx = oA + j * sa2;
          for (i = 0; i < Math.min(j, M); i++) {
            Av[idx] = alphaRe;
            Av[idx + 1] = alphaIm;
            idx += sa1;
          }
        }
        idx = oA;
        for (i = 0; i < mn; i++) {
          Av[idx] = betaRe;
          Av[idx + 1] = betaIm;
          idx += sa1 + sa2;
        }
      } else if (uplo === "L" || uplo === "l") {
        for (j = 0; j < Math.min(M, N); j++) {
          idx = oA + (j + 1) * sa1 + j * sa2;
          for (i = j + 1; i < M; i++) {
            Av[idx] = alphaRe;
            Av[idx + 1] = alphaIm;
            idx += sa1;
          }
        }
        idx = oA;
        for (i = 0; i < mn; i++) {
          Av[idx] = betaRe;
          Av[idx + 1] = betaIm;
          idx += sa1 + sa2;
        }
      } else {
        for (j = 0; j < N; j++) {
          idx = oA + j * sa2;
          for (i = 0; i < M; i++) {
            Av[idx] = alphaRe;
            Av[idx + 1] = alphaIm;
            idx += sa1;
          }
        }
        idx = oA;
        for (i = 0; i < mn; i++) {
          Av[idx] = betaRe;
          Av[idx + 1] = betaIm;
          idx += sa1 + sa2;
        }
      }
      return A;
    }
    module.exports = zlaset;
  }
});

// lib/lapack/base/zung2r/lib/base.js
var require_base46 = __commonJS({
  "lib/lapack/base/zung2r/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var reinterpret2 = require_lib57();
    var zlarf = require_base28();
    var zscal = require_base21();
    function zung2r(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var negTau;
      var tauv;
      var sa1;
      var sa2;
      var oA;
      var st;
      var Av;
      var ia;
      var it;
      var i;
      var j;
      var l;
      if (N <= 0) {
        return 0;
      }
      Av = reinterpret2(A, 0);
      tauv = reinterpret2(TAU, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      st = strideTAU * 2;
      for (j = K; j < N; j++) {
        for (l = 0; l < M; l++) {
          ia = oA + l * sa1 + j * sa2;
          Av[ia] = 0;
          Av[ia + 1] = 0;
        }
        ia = oA + j * sa1 + j * sa2;
        Av[ia] = 1;
        Av[ia + 1] = 0;
      }
      for (i = K - 1; i >= 0; i--) {
        it = offsetTAU * 2 + i * st;
        if (i < N - 1) {
          ia = oA + i * sa1 + i * sa2;
          Av[ia] = 1;
          Av[ia + 1] = 0;
          zlarf(
            "L",
            M - i,
            N - i - 1,
            A,
            strideA1,
            offsetA + i * strideA1 + i * strideA2,
            TAU,
            offsetTAU + i * strideTAU,
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
          negTau = new Complex128(-tauv[it], -tauv[it + 1]);
          zscal(
            M - i - 1,
            negTau,
            A,
            strideA1,
            offsetA + (i + 1) * strideA1 + i * strideA2
          );
        }
        ia = oA + i * sa1 + i * sa2;
        Av[ia] = 1 - tauv[it];
        Av[ia + 1] = -tauv[it + 1];
        for (l = 0; l < i; l++) {
          ia = oA + l * sa1 + i * sa2;
          Av[ia] = 0;
          Av[ia + 1] = 0;
        }
      }
      return 0;
    }
    module.exports = zung2r;
  }
});

// lib/lapack/base/zungqr/lib/base.js
var require_base47 = __commonJS({
  "lib/lapack/base/zungqr/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var zung2r = require_base46();
    var zlarft = require_base39();
    var zlarfb = require_base37();
    var NB = 32;
    function zungqr(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork) {
      var ldwork;
      var sa1;
      var sa2;
      var oA;
      var Av;
      var nb;
      var nx;
      var kk;
      var ki;
      var ib;
      var ia;
      var i;
      var j;
      var l;
      if (N <= 0) {
        return 0;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
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
              ia = oA + i * sa1 + j * sa2;
              Av[ia] = 0;
              Av[ia + 1] = 0;
            }
          }
        }
      } else {
        kk = 0;
      }
      if (kk < N) {
        zung2r(
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
          if (i + ib < N) {
            zlarft(
              "F",
              "C",
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
              1,
              ldwork,
              offsetWORK
            );
            zlarfb(
              "L",
              "N",
              "F",
              "C",
              M - i,
              N - i - ib,
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
              offsetA + i * strideA1 + (i + ib) * strideA2,
              WORK,
              1,
              ldwork,
              offsetWORK + ib
            );
          }
          zung2r(
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
            WORK,
            strideWORK,
            offsetWORK
          );
          for (j = i; j < i + ib; j++) {
            for (l = 0; l < i; l++) {
              ia = oA + l * sa1 + j * sa2;
              Av[ia] = 0;
              Av[ia + 1] = 0;
            }
          }
        }
      }
      return 0;
    }
    module.exports = zungqr;
  }
});

// lib/lapack/base/zungl2/lib/base.js
var require_base48 = __commonJS({
  "lib/lapack/base/zungl2/lib/base.js"(exports, module) {
    "use strict";
    var Complex128 = require_lib38();
    var Complex128Array2 = require_lib60();
    var reinterpret2 = require_lib57();
    var zlacgv = require_base29();
    var zlarf = require_base28();
    var zscal = require_base21();
    function zungl2(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK) {
      var negTau;
      var conjT;
      var tauv;
      var sa1;
      var sa2;
      var oA;
      var st;
      var Av;
      var ia;
      var it;
      var i;
      var j;
      var l;
      if (M <= 0) {
        return 0;
      }
      Av = reinterpret2(A, 0);
      tauv = reinterpret2(TAU, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
      st = strideTAU * 2;
      if (K < M) {
        for (j = 0; j < N; j++) {
          for (l = K; l < M; l++) {
            ia = oA + l * sa1 + j * sa2;
            Av[ia] = 0;
            Av[ia + 1] = 0;
          }
          if (j >= K && j < M) {
            ia = oA + j * sa1 + j * sa2;
            Av[ia] = 1;
            Av[ia + 1] = 0;
          }
        }
      }
      for (i = K - 1; i >= 0; i--) {
        it = offsetTAU * 2 + i * st;
        if (i < N - 1) {
          zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
          if (i < M - 1) {
            ia = oA + i * sa1 + i * sa2;
            Av[ia] = 1;
            Av[ia + 1] = 0;
            conjT = new Complex128(tauv[it], -tauv[it + 1]);
            zlarf(
              "R",
              M - i - 1,
              N - i,
              A,
              strideA2,
              offsetA + i * strideA1 + i * strideA2,
              new Complex128Array2([tauv[it], -tauv[it + 1]]),
              0,
              A,
              strideA1,
              strideA2,
              offsetA + (i + 1) * strideA1 + i * strideA2,
              WORK,
              strideWORK,
              offsetWORK
            );
          }
          negTau = new Complex128(-tauv[it], -tauv[it + 1]);
          zscal(
            N - i - 1,
            negTau,
            A,
            strideA2,
            offsetA + i * strideA1 + (i + 1) * strideA2
          );
          zlacgv(N - i - 1, A, strideA2, offsetA + i * strideA1 + (i + 1) * strideA2);
        }
        ia = oA + i * sa1 + i * sa2;
        Av[ia] = 1 - tauv[it];
        Av[ia + 1] = tauv[it + 1];
        for (l = 0; l < i; l++) {
          ia = oA + i * sa1 + l * sa2;
          Av[ia] = 0;
          Av[ia + 1] = 0;
        }
      }
      return 0;
    }
    module.exports = zungl2;
  }
});

// lib/lapack/base/zunglq/lib/base.js
var require_base49 = __commonJS({
  "lib/lapack/base/zunglq/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var zungl2 = require_base48();
    var zlarft = require_base39();
    var zlarfb = require_base37();
    var NB = 32;
    function zunglq(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork) {
      var ldwork;
      var sa1;
      var sa2;
      var oA;
      var Av;
      var nb;
      var nx;
      var kk;
      var ki;
      var ib;
      var ia;
      var i;
      var j;
      var l;
      if (M <= 0) {
        return 0;
      }
      Av = reinterpret2(A, 0);
      sa1 = strideA1 * 2;
      sa2 = strideA2 * 2;
      oA = offsetA * 2;
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
              ia = oA + i * sa1 + j * sa2;
              Av[ia] = 0;
              Av[ia + 1] = 0;
            }
          }
        }
      } else {
        kk = 0;
      }
      if (kk < M) {
        zungl2(
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
            zlarft(
              "F",
              "R",
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
            zlarfb(
              "R",
              "C",
              "F",
              "R",
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
          zungl2(
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
              ia = oA + l * sa1 + j * sa2;
              Av[ia] = 0;
              Av[ia + 1] = 0;
            }
          }
        }
      }
      return 0;
    }
    module.exports = zunglq;
  }
});

// lib/lapack/base/zungbr/lib/base.js
var require_base50 = __commonJS({
  "lib/lapack/base/zungbr/lib/base.js"(exports, module) {
    "use strict";
    var reinterpret2 = require_lib57();
    var zungqr = require_base47();
    var zunglq = require_base49();
    function zungbr(vect, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork) {
      var wantq;
      var Av;
      var sA1;
      var sA2;
      var oA;
      var idx;
      var i;
      var j;
      if (M === 0 || N === 0) {
        return 0;
      }
      wantq = vect === "Q" || vect === "q";
      Av = reinterpret2(A, 0);
      sA1 = strideA1 * 2;
      sA2 = strideA2 * 2;
      oA = offsetA * 2;
      if (wantq) {
        if (M >= K) {
          zungqr(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork);
        } else {
          for (j = M - 1; j >= 1; j--) {
            idx = oA + j * sA2;
            Av[idx] = 0;
            Av[idx + 1] = 0;
            for (i = j; i < M; i++) {
              idx = oA + i * sA1 + j * sA2;
              Av[idx] = Av[oA + i * sA1 + (j - 1) * sA2];
              Av[idx + 1] = Av[oA + i * sA1 + (j - 1) * sA2 + 1];
            }
          }
          Av[oA] = 1;
          Av[oA + 1] = 0;
          for (i = 1; i < M; i++) {
            idx = oA + i * sA1;
            Av[idx] = 0;
            Av[idx + 1] = 0;
          }
          if (M > 1) {
            zungqr(
              M - 1,
              M - 1,
              M - 1,
              A,
              strideA1,
              strideA2,
              offsetA + strideA1 + strideA2,
              TAU,
              strideTAU,
              offsetTAU,
              WORK,
              strideWORK,
              offsetWORK,
              lwork
            );
          }
        }
      } else {
        if (K < N) {
          zunglq(M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork);
        } else {
          Av[oA] = 1;
          Av[oA + 1] = 0;
          for (i = 1; i < N; i++) {
            idx = oA + i * sA1;
            Av[idx] = 0;
            Av[idx + 1] = 0;
          }
          for (j = 1; j < N; j++) {
            for (i = j - 1; i >= 1; i--) {
              idx = oA + i * sA1 + j * sA2;
              Av[idx] = Av[oA + (i - 1) * sA1 + j * sA2];
              Av[idx + 1] = Av[oA + (i - 1) * sA1 + j * sA2 + 1];
            }
            idx = oA + j * sA2;
            Av[idx] = 0;
            Av[idx + 1] = 0;
          }
          if (N > 1) {
            zunglq(
              N - 1,
              N - 1,
              N - 1,
              A,
              strideA1,
              strideA2,
              offsetA + strideA1 + strideA2,
              TAU,
              strideTAU,
              offsetTAU,
              WORK,
              strideWORK,
              offsetWORK,
              lwork
            );
          }
        }
      }
      return 0;
    }
    module.exports = zungbr;
  }
});

// lib/lapack/base/zgesvd/lib/base.js
var require_base51 = __commonJS({
  "lib/lapack/base/zgesvd/lib/base.js"(exports, module) {
    "use strict";
    var Complex128Array2 = require_lib60();
    var Complex128 = require_lib38();
    var dlamch = require_base();
    var dlascl = require_base2();
    var zbdsqr = require_base19();
    var zgebrd = require_base33();
    var zgeqrf = require_base40();
    var zlacpy = require_base41();
    var zlange = require_base43();
    var zlascl = require_base44();
    var zlaset = require_base45();
    var zungbr = require_base50();
    var CZERO = new Complex128(0, 0);
    function zgesvd2(jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK) {
      var wntua;
      var wntus;
      var wntuas;
      var wntuo;
      var wntun;
      var wntva;
      var wntvs;
      var wntvas;
      var wntvo;
      var wntvn;
      var minmn;
      var anrm;
      var bignum;
      var smlnum;
      var eps;
      var iscl;
      var info;
      var ierr;
      var ncu;
      var ncvt;
      var nru;
      var nrvt;
      var itau;
      var itauq;
      var itaup;
      var iwork;
      var irwork;
      var ie;
      var ir;
      var iu;
      var ldwrkr;
      var ldwrku;
      var chunk;
      var blk;
      var sa1;
      var sa2;
      var su1;
      var su2;
      var svt1;
      var svt2;
      var i;
      var WK;
      sa1 = strideA1;
      sa2 = strideA2;
      su1 = strideU1;
      su2 = strideU2;
      svt1 = strideVT1;
      svt2 = strideVT2;
      info = 0;
      minmn = Math.min(M, N);
      wntua = jobu === "A" || jobu === "a";
      wntus = jobu === "S" || jobu === "s";
      wntuas = wntua || wntus;
      wntuo = jobu === "O" || jobu === "o";
      wntun = jobu === "N" || jobu === "n";
      wntva = jobvt === "A" || jobvt === "a";
      wntvs = jobvt === "S" || jobvt === "s";
      wntvas = wntva || wntvs;
      wntvo = jobvt === "O" || jobvt === "o";
      wntvn = jobvt === "N" || jobvt === "n";
      if (M === 0 || N === 0) {
        return 0;
      }
      var wsz = Math.max(1, 3 * minmn + Math.max(M, N) + minmn * Math.max(M, N));
      if (lwork >= wsz) {
        WK = WORK;
      } else {
        WK = new Complex128Array2(wsz);
      }
      eps = dlamch("P");
      smlnum = Math.sqrt(dlamch("S")) / eps;
      bignum = 1 / smlnum;
      anrm = zlange("M", M, N, A, sa1, sa2, offsetA, RWORK, strideRWORK, offsetRWORK);
      iscl = 0;
      if (anrm > 0 && anrm < smlnum) {
        iscl = 1;
        zlascl("G", 0, 0, anrm, smlnum, M, N, A, sa1, sa2, offsetA);
      } else if (anrm > bignum) {
        iscl = 1;
        zlascl("G", 0, 0, anrm, bignum, M, N, A, sa1, sa2, offsetA);
      }
      if (M >= N) {
        if (wntun && M >= 2 * N) {
          itau = 0;
          iwork = itau + N;
          zgeqrf(
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
            zlaset("L", N - 1, N - 1, CZERO, CZERO, A, sa1, sa2, offsetA + sa1);
          }
          ie = 0;
          itauq = 0;
          itaup = itauq + N;
          iwork = itaup + N;
          zgebrd(
            N,
            N,
            A,
            sa1,
            sa2,
            offsetA,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
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
            zungbr(
              "P",
              N,
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              -1
            );
            ncvt = N;
          }
          irwork = ie + N;
          info = zbdsqr(
            "U",
            N,
            ncvt,
            0,
            0,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
            A,
            sa1,
            sa2,
            offsetA,
            A,
            sa1,
            sa2,
            offsetA,
            // U dummy (NRU=0)
            A,
            sa1,
            sa2,
            offsetA,
            // C dummy (NCC=0)
            RWORK,
            strideRWORK,
            offsetRWORK + irwork
          );
          if (wntvas) {
            zlacpy("F", N, N, A, sa1, sa2, offsetA, VT, svt1, svt2, offsetVT);
          }
        } else {
          ie = 0;
          itauq = 0;
          itaup = itauq + N;
          iwork = itaup + N;
          zgebrd(
            M,
            N,
            A,
            sa1,
            sa2,
            offsetA,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
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
            zlacpy("L", M, N, A, sa1, sa2, offsetA, U, su1, su2, offsetU);
            ncu = wntus ? N : M;
            zungbr(
              "Q",
              M,
              ncu,
              N,
              U,
              su1,
              su2,
              offsetU,
              WK,
              1,
              itauq,
              WK,
              1,
              iwork,
              -1
            );
          }
          if (wntvas) {
            zlacpy("U", N, N, A, sa1, sa2, offsetA, VT, svt1, svt2, offsetVT);
            zungbr(
              "P",
              N,
              N,
              N,
              VT,
              svt1,
              svt2,
              offsetVT,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              -1
            );
          }
          if (wntuo) {
            zungbr(
              "Q",
              M,
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itauq,
              WK,
              1,
              iwork,
              -1
            );
          }
          if (wntvo) {
            zungbr(
              "P",
              N,
              N,
              N,
              A,
              sa1,
              sa2,
              offsetA,
              WK,
              1,
              itaup,
              WK,
              1,
              iwork,
              -1
            );
          }
          irwork = ie + N;
          nru = 0;
          ncvt = 0;
          if (wntuas || wntuo) {
            nru = M;
          }
          if (wntvas || wntvo) {
            ncvt = N;
          }
          if (!wntuo && !wntvo) {
            info = zbdsqr(
              "U",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              RWORK,
              strideRWORK,
              offsetRWORK + ie,
              VT,
              svt1,
              svt2,
              offsetVT,
              U,
              su1,
              su2,
              offsetU,
              A,
              sa1,
              sa2,
              offsetA,
              // C dummy (NCC=0)
              RWORK,
              strideRWORK,
              offsetRWORK + irwork
            );
          } else if (!wntuo && wntvo) {
            info = zbdsqr(
              "U",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              RWORK,
              strideRWORK,
              offsetRWORK + ie,
              A,
              sa1,
              sa2,
              offsetA,
              U,
              su1,
              su2,
              offsetU,
              A,
              sa1,
              sa2,
              offsetA,
              // C dummy (NCC=0)
              RWORK,
              strideRWORK,
              offsetRWORK + irwork
            );
          } else {
            info = zbdsqr(
              "U",
              N,
              ncvt,
              nru,
              0,
              s,
              strideS,
              offsetS,
              RWORK,
              strideRWORK,
              offsetRWORK + ie,
              VT,
              svt1,
              svt2,
              offsetVT,
              A,
              sa1,
              sa2,
              offsetA,
              A,
              sa1,
              sa2,
              offsetA,
              // C dummy (NCC=0)
              RWORK,
              strideRWORK,
              offsetRWORK + irwork
            );
          }
        }
      } else {
        ie = 0;
        itauq = 0;
        itaup = itauq + M;
        iwork = itaup + M;
        zgebrd(
          M,
          N,
          A,
          sa1,
          sa2,
          offsetA,
          s,
          strideS,
          offsetS,
          RWORK,
          strideRWORK,
          offsetRWORK + ie,
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
          zlacpy("L", M, M, A, sa1, sa2, offsetA, U, su1, su2, offsetU);
          zungbr(
            "Q",
            M,
            M,
            N,
            U,
            su1,
            su2,
            offsetU,
            WK,
            1,
            itauq,
            WK,
            1,
            iwork,
            -1
          );
        }
        if (wntvas) {
          zlacpy("U", M, N, A, sa1, sa2, offsetA, VT, svt1, svt2, offsetVT);
          nrvt = wntva ? N : M;
          zungbr(
            "P",
            nrvt,
            N,
            M,
            VT,
            svt1,
            svt2,
            offsetVT,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            -1
          );
        }
        if (wntuo) {
          zungbr(
            "Q",
            M,
            M,
            N,
            A,
            sa1,
            sa2,
            offsetA,
            WK,
            1,
            itauq,
            WK,
            1,
            iwork,
            -1
          );
        }
        if (wntvo) {
          zungbr(
            "P",
            M,
            N,
            M,
            A,
            sa1,
            sa2,
            offsetA,
            WK,
            1,
            itaup,
            WK,
            1,
            iwork,
            -1
          );
        }
        irwork = ie + M;
        nru = 0;
        ncvt = 0;
        if (wntuas || wntuo) {
          nru = M;
        }
        if (wntvas || wntvo) {
          ncvt = N;
        }
        if (!wntuo && !wntvo) {
          info = zbdsqr(
            "L",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
            VT,
            svt1,
            svt2,
            offsetVT,
            U,
            su1,
            su2,
            offsetU,
            A,
            sa1,
            sa2,
            offsetA,
            RWORK,
            strideRWORK,
            offsetRWORK + irwork
          );
        } else if (!wntuo && wntvo) {
          info = zbdsqr(
            "L",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
            A,
            sa1,
            sa2,
            offsetA,
            U,
            su1,
            su2,
            offsetU,
            A,
            sa1,
            sa2,
            offsetA,
            RWORK,
            strideRWORK,
            offsetRWORK + irwork
          );
        } else {
          info = zbdsqr(
            "L",
            M,
            ncvt,
            nru,
            0,
            s,
            strideS,
            offsetS,
            RWORK,
            strideRWORK,
            offsetRWORK + ie,
            VT,
            svt1,
            svt2,
            offsetVT,
            A,
            sa1,
            sa2,
            offsetA,
            A,
            sa1,
            sa2,
            offsetA,
            RWORK,
            strideRWORK,
            offsetRWORK + irwork
          );
        }
      }
      if (iscl === 1) {
        if (anrm > bignum) {
          dlascl("G", 0, 0, bignum, anrm, minmn, 1, s, strideS, 1, offsetS);
        }
        if (info !== 0 && anrm > bignum) {
          dlascl("G", 0, 0, bignum, anrm, minmn - 1, 1, RWORK, strideRWORK, 1, offsetRWORK + ie);
        }
        if (anrm < smlnum) {
          dlascl("G", 0, 0, smlnum, anrm, minmn, 1, s, strideS, 1, offsetS);
        }
        if (info !== 0 && anrm < smlnum) {
          dlascl("G", 0, 0, smlnum, anrm, minmn - 1, 1, RWORK, strideRWORK, 1, offsetRWORK + ie);
        }
      }
      return info;
    }
    module.exports = zgesvd2;
  }
});

// lib/lapack/base/zgesvd/lib/ndarray.js
var require_ndarray = __commonJS({
  "lib/lapack/base/zgesvd/lib/ndarray.js"(exports, module) {
    "use strict";
    var base = require_base51();
    function zgesvd2(jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK) {
      return base(jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK);
    }
    module.exports = zgesvd2;
  }
});

// notebooks/laplace/aaa-entry.js
var import_complex128 = __toESM(require_lib60(), 1);
var import_reinterpret_complex128 = __toESM(require_lib57(), 1);

// qz.js
var SAFMIN = Number.MIN_VALUE * (1 / Number.EPSILON);
var SAFMAX = 1 / SAFMIN;
var ULP = Number.EPSILON;
var BASE = 2;
function cadd(out, a, b) {
  out[0] = a[0] + b[0];
  out[1] = a[1] + b[1];
  return out;
}
function csub(out, a, b) {
  out[0] = a[0] - b[0];
  out[1] = a[1] - b[1];
  return out;
}
function cmul(out, a, b) {
  const ar = a[0], ai = a[1], br = b[0], bi = b[1];
  out[0] = ar * br - ai * bi;
  out[1] = ar * bi + ai * br;
  return out;
}
function cscale(out, s, a) {
  out[0] = s * a[0];
  out[1] = s * a[1];
  return out;
}
function cset(out, re, im) {
  out[0] = re;
  out[1] = im;
  return out;
}
function ccopy(out, a) {
  out[0] = a[0];
  out[1] = a[1];
  return out;
}
function cabs(a) {
  return Math.sqrt(a[0] * a[0] + a[1] * a[1]);
}
function cabs1(a) {
  return Math.abs(a[0]) + Math.abs(a[1]);
}
function grot(a, b, c, sr, si) {
  const ar = a[0], ai = a[1], br = b[0], bi = b[1];
  a[0] = c * ar + sr * br - si * bi;
  a[1] = c * ai + sr * bi + si * br;
  b[0] = c * br - sr * ar - si * ai;
  b[1] = c * bi - sr * ai + si * ar;
}
var _s = [0, 0];
var _r = [0, 0];
var _t = [0, 0];
var _u = [0, 0];
var _v = [0, 0];
var _w = [0, 0];
var _t2 = [0, 0];
var _t3 = [0, 0];
var _t4 = [0, 0];
var _t5 = [0, 0];
var _t6 = [0, 0];
var _t7 = [0, 0];
var _t8 = [0, 0];
var _t9 = [0, 0];
function dladiv2(a, b, c, d, r, t) {
  if (r !== 0) {
    const br = b * r;
    if (br !== 0) return (a + br) * t;
    return a * t + b * t * r;
  }
  return (a + d * (b / c)) * t;
}
function zladiv(out, x, y) {
  const a = x[0], b = x[1], c = y[0], d = y[1];
  const ov = Number.MAX_VALUE;
  const un = SAFMIN;
  const eps = ULP;
  const bs = 2;
  const be = bs / (eps * eps);
  let aa = a, bb = b, cc = c, dd = d, s = 1;
  const ab = Math.max(Math.abs(a), Math.abs(b));
  const cd = Math.max(Math.abs(c), Math.abs(d));
  if (ab >= 0.5 * ov) {
    aa *= 0.5;
    bb *= 0.5;
    s *= 2;
  }
  if (cd >= 0.5 * ov) {
    cc *= 0.5;
    dd *= 0.5;
    s *= 0.5;
  }
  if (ab <= un * bs / eps) {
    aa *= be;
    bb *= be;
    s /= be;
  }
  if (cd <= un * bs / eps) {
    cc *= be;
    dd *= be;
    s *= be;
  }
  let p, q;
  if (Math.abs(d) <= Math.abs(c)) {
    const r = dd / cc;
    const t = 1 / (cc + dd * r);
    p = dladiv2(aa, bb, cc, dd, r, t);
    q = dladiv2(bb, -aa, cc, dd, r, t);
  } else {
    const r = cc / dd;
    const t = 1 / (dd + cc * r);
    p = dladiv2(bb, aa, dd, cc, r, t);
    q = -dladiv2(aa, -bb, dd, cc, r, t);
  }
  out[0] = p * s;
  out[1] = q * s;
  return out;
}
function zlartg(s_out, r_out, f, g) {
  const fr = f[0], fi = f[1], gr = g[0], gi = g[1];
  const abssq = (r, i) => r * r + i * i;
  const rtmin = Math.sqrt(SAFMIN);
  if (gr === 0 && gi === 0) {
    cset(s_out, 0, 0);
    cset(r_out, fr, fi);
    return 1;
  }
  if (fr === 0 && fi === 0) {
    if (fi === 0 && gi === 0) {
      const d = Math.abs(gr);
      cset(r_out, d, 0);
      cset(s_out, gr / d, -gi / d);
    } else if (fr === 0 && gr === 0) {
      const d = Math.abs(gi);
      cset(r_out, d, 0);
      cset(s_out, gr / d, -gi / d);
    } else {
      const g12 = Math.max(Math.abs(gr), Math.abs(gi));
      const rtmax2 = Math.sqrt(SAFMAX / 2);
      if (g12 > rtmin && g12 < rtmax2) {
        const g2 = abssq(gr, gi);
        const d = Math.sqrt(g2);
        cset(s_out, gr / d, -gi / d);
        cset(r_out, d, 0);
      } else {
        const u = Math.min(SAFMAX, Math.max(SAFMIN, g12));
        const gsr = gr / u, gsi = gi / u;
        const g2 = abssq(gsr, gsi);
        const d = Math.sqrt(g2);
        cset(s_out, gsr / d, -gsi / d);
        cset(r_out, d * u, 0);
      }
    }
    return 0;
  }
  const f1 = Math.max(Math.abs(fr), Math.abs(fi));
  const g1 = Math.max(Math.abs(gr), Math.abs(gi));
  let rtmax = Math.sqrt(SAFMAX / 4);
  let c;
  if (f1 > rtmin && f1 < rtmax && g1 > rtmin && g1 < rtmax) {
    const f2 = abssq(fr, fi);
    const g2 = abssq(gr, gi);
    const h2 = f2 + g2;
    if (f2 >= h2 * SAFMIN) {
      c = Math.sqrt(f2 / h2);
      cset(r_out, fr / c, fi / c);
      rtmax *= 2;
      if (f2 > rtmin && h2 < rtmax) {
        const k = 1 / Math.sqrt(f2 * h2);
        s_out[0] = (gr * fr + gi * fi) * k;
        s_out[1] = (-gi * fr + gr * fi) * k;
      } else {
        const rx = r_out[0] / h2, ry = r_out[1] / h2;
        s_out[0] = gr * rx + gi * ry;
        s_out[1] = -gi * rx + gr * ry;
      }
    } else {
      const d = Math.sqrt(f2 * h2);
      c = f2 / d;
      if (c >= SAFMIN) {
        cset(r_out, fr / c, fi / c);
      } else {
        cset(r_out, fr * (h2 / d), fi * (h2 / d));
      }
      const k = 1 / d;
      s_out[0] = (gr * fr + gi * fi) * k;
      s_out[1] = (-gi * fr + gr * fi) * k;
    }
  } else {
    const u = Math.min(SAFMAX, Math.max(SAFMIN, f1, g1));
    const gsr = gr / u, gsi = gi / u;
    const g2 = abssq(gsr, gsi);
    let f2, fsr, fsi, w;
    if (f1 / u < rtmin) {
      const v = Math.min(SAFMAX, Math.max(SAFMIN, f1));
      w = v / u;
      fsr = fr / v;
      fsi = fi / v;
      f2 = abssq(fsr, fsi);
    } else {
      w = 1;
      fsr = fr / u;
      fsi = fi / u;
      f2 = abssq(fsr, fsi);
    }
    const h2 = f2 * w * w + g2;
    if (f2 >= h2 * SAFMIN) {
      c = Math.sqrt(f2 / h2);
      cset(r_out, fsr / c, fsi / c);
      rtmax *= 2;
      if (f2 > rtmin && h2 < rtmax) {
        const k = 1 / Math.sqrt(f2 * h2);
        s_out[0] = (gsr * fsr + gsi * fsi) * k;
        s_out[1] = (-gsi * fsr + gsr * fsi) * k;
      } else {
        const rx = r_out[0] / h2, ry = r_out[1] / h2;
        s_out[0] = gsr * rx + gsi * ry;
        s_out[1] = -gsi * rx + gsr * ry;
      }
    } else {
      const d = Math.sqrt(f2 * h2);
      c = f2 / d;
      if (c >= SAFMIN) {
        cset(r_out, fsr / c, fsi / c);
      } else {
        cset(r_out, fsr * (h2 / d), fsi * (h2 / d));
      }
      const k = 1 / d;
      s_out[0] = (gsr * fsr + gsi * fsi) * k;
      s_out[1] = (-gsi * fsr + gsr * fsi) * k;
    }
    c *= w;
    r_out[0] *= u;
    r_out[1] *= u;
  }
  return c;
}
function zlanhs_frobenius(H, n, ilo, ihi) {
  let scale = 0, sumsq = 1;
  for (let j = ilo; j <= ihi; j++) {
    for (let i = ilo; i <= Math.min(ihi, j + 1); i++) {
      const val = cabs(H[i][j]);
      if (val !== 0) {
        if (scale < val) {
          sumsq = 1 + sumsq * (scale / val) ** 2;
          scale = val;
        } else {
          sumsq += (val / scale) ** 2;
        }
      }
    }
  }
  return scale * Math.sqrt(sumsq);
}
function zgghrd(A, B, n, ilo, ihi) {
  for (let j = 0; j < n - 1; j++)
    for (let i = j + 1; i < n; i++)
      cset(B[i][j], 0, 0);
  for (let jcol = ilo; jcol <= ihi - 2; jcol++) {
    for (let jrow = ihi; jrow >= jcol + 2; jrow--) {
      const c1 = zlartg(_s, A[jrow - 1][jcol], A[jrow - 1][jcol], A[jrow][jcol]);
      cset(A[jrow][jcol], 0, 0);
      const s1r = _s[0], s1i = _s[1];
      for (let j = jcol + 1; j < n; j++) grot(A[jrow - 1][j], A[jrow][j], c1, s1r, s1i);
      for (let j = jrow - 1; j < n; j++) grot(B[jrow - 1][j], B[jrow][j], c1, s1r, s1i);
      const c2 = zlartg(_s, B[jrow][jrow], B[jrow][jrow], B[jrow][jrow - 1]);
      cset(B[jrow][jrow - 1], 0, 0);
      const s2r = _s[0], s2i = _s[1];
      for (let j = 0; j <= ihi; j++) grot(A[j][jrow], A[j][jrow - 1], c2, s2r, s2i);
      for (let j = 0; j < jrow; j++) grot(B[j][jrow], B[j][jrow - 1], c2, s2r, s2i);
    }
  }
}
function zhgeqz(H, T, n, ilo, ihi) {
  const alpha = new Array(n);
  const beta = new Array(n);
  for (let i = 0; i < n; i++) {
    alpha[i] = [0, 0];
    beta[i] = [0, 0];
  }
  if (n === 0) return { alpha, beta, info: 0 };
  const safmin = SAFMIN;
  const ulp = ULP * BASE;
  const anorm = zlanhs_frobenius(H, n, ilo, ihi);
  const bnorm = zlanhs_frobenius(T, n, ilo, ihi);
  const atol = Math.max(safmin, ulp * anorm);
  const btol = Math.max(safmin, ulp * bnorm);
  const ascale = 1 / Math.max(safmin, anorm);
  const bscale = 1 / Math.max(safmin, bnorm);
  for (let j = ihi + 1; j < n; j++) {
    const absb = cabs(T[j][j]);
    if (absb > safmin) {
      const k = 1 / absb;
      _t[0] = T[j][j][0] * k;
      _t[1] = -T[j][j][1] * k;
      cset(T[j][j], absb, 0);
      cmul(H[j][j], _t, H[j][j]);
    } else {
      cset(T[j][j], 0, 0);
    }
    ccopy(alpha[j], H[j][j]);
    ccopy(beta[j], T[j][j]);
  }
  if (ihi < ilo) {
    for (let j = 0; j < ilo; j++) {
      ccopy(alpha[j], H[j][j]);
      ccopy(beta[j], T[j][j]);
    }
    return { alpha, beta, info: 0 };
  }
  let ilast = ihi;
  let ifrstm = ilo;
  let ilastm = ihi;
  let iiter = 0;
  const eshift = [0, 0];
  const maxit = 30 * (ihi - ilo + 1);
  let info = 0;
  for (let jiter = 1; jiter <= maxit; jiter++) {
    let converged = false;
    let tZeroAtIlast = false;
    let ifirst = -1;
    if (ilast === ilo) {
      converged = true;
    } else if (cabs1(H[ilast][ilast - 1]) <= Math.max(
      safmin,
      ulp * (cabs1(H[ilast][ilast]) + cabs1(H[ilast - 1][ilast - 1]))
    )) {
      cset(H[ilast][ilast - 1], 0, 0);
      converged = true;
    }
    if (!converged && cabs(T[ilast][ilast]) <= btol) {
      cset(T[ilast][ilast], 0, 0);
      tZeroAtIlast = true;
    }
    if (!converged && !tZeroAtIlast) {
      let found = false;
      for (let j = ilast - 1; j >= ilo; j--) {
        let ilazro;
        if (j === ilo) {
          ilazro = true;
        } else if (cabs1(H[j][j - 1]) <= Math.max(
          safmin,
          ulp * (cabs1(H[j][j]) + cabs1(H[j - 1][j - 1]))
        )) {
          cset(H[j][j - 1], 0, 0);
          ilazro = true;
        } else {
          ilazro = false;
        }
        if (cabs(T[j][j]) < btol) {
          cset(T[j][j], 0, 0);
          let ilazr2 = false;
          if (!ilazro && cabs1(H[j][j - 1]) * (ascale * cabs1(H[j + 1][j])) <= cabs1(H[j][j]) * (ascale * atol)) {
            ilazr2 = true;
          }
          if (ilazro || ilazr2) {
            for (let jch = j; jch <= ilast - 1; jch++) {
              const gc = zlartg(_s, H[jch][jch], H[jch][jch], H[jch + 1][jch]);
              cset(H[jch + 1][jch], 0, 0);
              const sr2 = _s[0], si2 = _s[1];
              for (let jc = jch + 1; jc <= ilastm; jc++) grot(H[jch][jc], H[jch + 1][jc], gc, sr2, si2);
              for (let jc = jch + 1; jc <= ilastm; jc++) grot(T[jch][jc], T[jch + 1][jc], gc, sr2, si2);
              if (ilazr2) cscale(H[jch][jch - 1], gc, H[jch][jch - 1]);
              ilazr2 = false;
              if (cabs1(T[jch + 1][jch + 1]) >= btol) {
                if (jch + 1 >= ilast) converged = true;
                else ifirst = jch + 1;
                found = true;
                break;
              }
              cset(T[jch + 1][jch + 1], 0, 0);
            }
            if (!found) tZeroAtIlast = true;
            found = true;
            break;
          } else {
            for (let jch = j; jch <= ilast - 1; jch++) {
              const gc1 = zlartg(_s, T[jch][jch + 1], T[jch][jch + 1], T[jch + 1][jch + 1]);
              cset(T[jch + 1][jch + 1], 0, 0);
              const s1r = _s[0], s1i = _s[1];
              if (jch < ilastm - 1) for (let jc = jch + 2; jc <= ilastm; jc++) grot(T[jch][jc], T[jch + 1][jc], gc1, s1r, s1i);
              for (let jc = jch - 1; jc <= ilastm; jc++) grot(H[jch][jc], H[jch + 1][jc], gc1, s1r, s1i);
              const gc2 = zlartg(_s, H[jch + 1][jch], H[jch + 1][jch], H[jch + 1][jch - 1]);
              cset(H[jch + 1][jch - 1], 0, 0);
              const s2r = _s[0], s2i = _s[1];
              for (let jr = ifrstm; jr <= jch + 1; jr++) grot(H[jr][jch], H[jr][jch - 1], gc2, s2r, s2i);
              for (let jr = ifrstm; jr <= jch; jr++) grot(T[jr][jch], T[jr][jch - 1], gc2, s2r, s2i);
            }
            tZeroAtIlast = true;
            found = true;
            break;
          }
        } else if (ilazro) {
          ifirst = j;
          found = true;
          break;
        }
      }
      if (!found) {
        info = 2 * n + 1;
        break;
      }
    }
    if (tZeroAtIlast && !converged) {
      const gc = zlartg(_s, H[ilast][ilast], H[ilast][ilast], H[ilast][ilast - 1]);
      cset(H[ilast][ilast - 1], 0, 0);
      const sr2 = _s[0], si2 = _s[1];
      for (let jr = ifrstm; jr < ilast; jr++) grot(H[jr][ilast], H[jr][ilast - 1], gc, sr2, si2);
      for (let jr = ifrstm; jr < ilast; jr++) grot(T[jr][ilast], T[jr][ilast - 1], gc, sr2, si2);
      converged = true;
    }
    if (converged) {
      const absb = cabs(T[ilast][ilast]);
      if (absb > safmin) {
        const k = 1 / absb;
        _t[0] = T[ilast][ilast][0] * k;
        _t[1] = -T[ilast][ilast][1] * k;
        cset(T[ilast][ilast], absb, 0);
        cmul(H[ilast][ilast], _t, H[ilast][ilast]);
      } else {
        cset(T[ilast][ilast], 0, 0);
      }
      ccopy(alpha[ilast], H[ilast][ilast]);
      ccopy(beta[ilast], T[ilast][ilast]);
      ilast--;
      if (ilast < ilo) break;
      iiter = 0;
      cset(eshift, 0, 0);
      ilastm = ilast;
      if (ifrstm > ilast) ifrstm = ilo;
      continue;
    }
    iiter++;
    ifrstm = ifirst;
    const shift = _t5;
    if (iiter % 10 !== 0) {
      const u12 = _t2, ad11 = _t3, ad21 = _t4, ad12 = _t6, ad22 = _t7;
      cscale(_t, bscale, T[ilast - 1][ilast]);
      cscale(_u, bscale, T[ilast][ilast]);
      zladiv(u12, _t, _u);
      cscale(_t, ascale, H[ilast - 1][ilast - 1]);
      cscale(_u, bscale, T[ilast - 1][ilast - 1]);
      zladiv(ad11, _t, _u);
      cscale(_t, ascale, H[ilast][ilast - 1]);
      zladiv(ad21, _t, _u);
      cscale(_t, ascale, H[ilast - 1][ilast]);
      cscale(_u, bscale, T[ilast][ilast]);
      zladiv(ad12, _t, _u);
      cscale(_t, ascale, H[ilast][ilast]);
      zladiv(ad22, _t, _u);
      cmul(_t, u12, ad21);
      csub(_t8, ad22, _t);
      cmul(_t, u12, ad11);
      csub(_t9, ad12, _t);
      ccopy(shift, _t8);
      csqrt(_t, _t9);
      csqrt(_u, ad21);
      cmul(_v, _t, _u);
      const ctemp_abs1 = cabs1(_v);
      if (ctemp_abs1 !== 0) {
        csub(_t, ad11, shift);
        cscale(_w, 0.5, _t);
        const x_abs1 = cabs1(_w);
        const tempMax = Math.max(ctemp_abs1, x_abs1);
        const invMax = 1 / tempMax;
        cscale(_t, invMax, _w);
        cmul(_t, _t, _t);
        cscale(_u, invMax, _v);
        cmul(_u, _u, _u);
        cadd(_t, _t, _u);
        csqrt(_t, _t);
        cscale(_t, tempMax, _t);
        if (x_abs1 > 0) {
          cscale(_u, 1 / x_abs1, _w);
          if (_u[0] * _t[0] + _u[1] * _t[1] < 0) {
            _t[0] = -_t[0];
            _t[1] = -_t[1];
          }
        }
        cadd(_t, _w, _t);
        zladiv(_t, _v, _t);
        cmul(_t, _v, _t);
        csub(shift, shift, _t);
      }
    } else {
      if (iiter % 20 === 0 && bscale * cabs1(T[ilast][ilast]) > safmin) {
        cscale(_t, ascale, H[ilast][ilast]);
        cscale(_u, bscale, T[ilast][ilast]);
        zladiv(_t, _t, _u);
        cadd(eshift, eshift, _t);
      } else {
        cscale(_t, ascale, H[ilast][ilast - 1]);
        cscale(_u, bscale, T[ilast - 1][ilast - 1]);
        zladiv(_t, _t, _u);
        cadd(eshift, eshift, _t);
      }
      ccopy(shift, eshift);
    }
    let istart = ifirst;
    cscale(_t, ascale, H[ifirst][ifirst]);
    cscale(_u, bscale, T[ifirst][ifirst]);
    cmul(_u, shift, _u);
    csub(_t, _t, _u);
    for (let j = ilast - 1; j >= ifirst + 1; j--) {
      cscale(_u, ascale, H[j][j]);
      cscale(_v, bscale, T[j][j]);
      cmul(_v, shift, _v);
      csub(_u, _u, _v);
      const temp = cabs1(_u);
      const temp2 = ascale * cabs1(H[j + 1][j]);
      const tempr = Math.max(temp, temp2);
      let t1 = temp, t2 = temp2;
      if (tempr < 1 && tempr !== 0) {
        t1 /= tempr;
        t2 /= tempr;
      }
      if (cabs1(H[j][j - 1]) * t2 <= t1 * atol) {
        istart = j;
        ccopy(_t, _u);
        break;
      }
    }
    cscale(_u, ascale, H[istart + 1][istart]);
    let c = zlartg(_s, _r, _t, _u);
    let sr = _s[0], si = _s[1];
    for (let j = istart; j <= ilast - 1; j++) {
      if (j > istart) {
        c = zlartg(_s, H[j][j - 1], H[j][j - 1], H[j + 1][j - 1]);
        cset(H[j + 1][j - 1], 0, 0);
        sr = _s[0];
        si = _s[1];
      }
      for (let jc = j; jc <= ilastm; jc++) grot(H[j][jc], H[j + 1][jc], c, sr, si);
      for (let jc = j; jc <= ilastm; jc++) grot(T[j][jc], T[j + 1][jc], c, sr, si);
      const c2 = zlartg(_s, T[j + 1][j + 1], T[j + 1][j + 1], T[j + 1][j]);
      cset(T[j + 1][j], 0, 0);
      const s2r = _s[0], s2i = _s[1];
      for (let jr = ifrstm; jr <= Math.min(j + 2, ilast); jr++) grot(H[jr][j + 1], H[jr][j], c2, s2r, s2i);
      for (let jr = ifrstm; jr <= j; jr++) grot(T[jr][j + 1], T[jr][j], c2, s2r, s2i);
    }
  }
  for (let j = 0; j < ilo; j++) {
    const absb = cabs(T[j][j]);
    if (absb > safmin) {
      const k = 1 / absb;
      _t[0] = T[j][j][0] * k;
      _t[1] = -T[j][j][1] * k;
      cset(T[j][j], absb, 0);
      cmul(H[j][j], _t, H[j][j]);
    } else {
      cset(T[j][j], 0, 0);
    }
    ccopy(alpha[j], H[j][j]);
    ccopy(beta[j], T[j][j]);
  }
  if (info === 0 && ilast >= ilo) info = ilast + 1;
  return { alpha, beta, info };
}
function csqrt(out, z) {
  const ar = z[0], ai = z[1];
  const r = Math.sqrt(ar * ar + ai * ai);
  if (r === 0) {
    cset(out, 0, 0);
    return out;
  }
  const s = ai >= 0 ? 1 : -1;
  out[0] = Math.sqrt((r + ar) / 2);
  out[1] = s * Math.sqrt((r - ar) / 2);
  return out;
}
function zggev(A, B, n) {
  if (n === 0) return [];
  const H = A.map((row) => row.map((v) => [v[0], v[1]]));
  const T = B.map((row) => row.map((v) => [v[0], v[1]]));
  zgghrd(H, T, n, 0, n - 1);
  const { alpha, beta } = zhgeqz(H, T, n, 0, n - 1);
  const eigenvalues = [];
  for (let i = 0; i < n; i++) {
    if (cabs(beta[i]) > SAFMIN) {
      const eig = [0, 0];
      zladiv(eig, alpha[i], beta[i]);
      eigenvalues.push(eig);
    }
  }
  return eigenvalues;
}

// prz.js
function barycentricRoots(z, weights) {
  const m = z.length;
  if (m <= 1) return [];
  const n = m + 1;
  const E = [];
  for (let i = 0; i < n; i++) {
    E[i] = [];
    for (let j = 0; j < n; j++) E[i][j] = [0, 0];
  }
  for (let j = 0; j < m; j++) {
    E[0][j + 1][0] = weights[j][0];
    E[0][j + 1][1] = weights[j][1];
  }
  for (let i = 0; i < m; i++) cset(E[i + 1][0], 1, 0);
  for (let i = 0; i < m; i++) {
    E[i + 1][i + 1][0] = z[i][0];
    E[i + 1][i + 1][1] = z[i][1];
  }
  const B = [];
  for (let i = 0; i < n; i++) {
    B[i] = [];
    for (let j = 0; j < n; j++) B[i][j] = [0, 0];
  }
  for (let i = 1; i < n; i++) cset(B[i][i], 1, 0);
  return zggev(E, B, n);
}
function feval(zz, z, f, w) {
  const result = new Array(zz.length);
  for (let idx = 0; idx < zz.length; idx++) {
    const zv = zz[idx];
    let exact = -1;
    for (let j = 0; j < z.length; j++) {
      if (zv[0] === z[j][0] && zv[1] === z[j][1]) {
        exact = j;
        break;
      }
    }
    if (exact >= 0) {
      result[idx] = [f[exact][0], f[exact][1]];
      continue;
    }
    let nr = 0, ni = 0, dr = 0, di = 0;
    for (let j = 0; j < z.length; j++) {
      const dx = zv[0] - z[j][0], dy = zv[1] - z[j][1];
      const d = dx * dx + dy * dy;
      const invr = dx / d, invi = -dy / d;
      const wfr = w[j][0] * f[j][0] - w[j][1] * f[j][1];
      const wfi = w[j][0] * f[j][1] + w[j][1] * f[j][0];
      nr += wfr * invr - wfi * invi;
      ni += wfr * invi + wfi * invr;
      dr += w[j][0] * invr - w[j][1] * invi;
      di += w[j][0] * invi + w[j][1] * invr;
    }
    const dd = dr * dr + di * di;
    result[idx] = [(nr * dr + ni * di) / dd, (ni * dr - nr * di) / dd];
  }
  return result;
}
function prz(z, f, w) {
  const pol = barycentricRoots(z, w);
  const wf = new Array(w.length);
  for (let i = 0; i < w.length; i++) {
    wf[i] = [0, 0];
    cmul(wf[i], w[i], f[i]);
  }
  const zer = barycentricRoots(z, wf);
  const dz = new Array(4);
  for (let k = 0; k < 4; k++) {
    const theta = 2 * Math.PI * (k + 1) / 4;
    dz[k] = [1e-5 * Math.cos(theta), 1e-5 * Math.sin(theta)];
  }
  const res = new Array(pol.length);
  for (let j = 0; j < pol.length; j++) {
    const pts = new Array(4);
    for (let k = 0; k < 4; k++) {
      pts[k] = [pol[j][0] + dz[k][0], pol[j][1] + dz[k][1]];
    }
    const vals = feval(pts, z, f, w);
    let rr = 0, ri = 0;
    for (let k = 0; k < 4; k++) {
      rr += vals[k][0] * dz[k][0] - vals[k][1] * dz[k][1];
      ri += vals[k][0] * dz[k][1] + vals[k][1] * dz[k][0];
    }
    res[j] = [0.25 * rr, 0.25 * ri];
  }
  return { pol, res, zer };
}
function cleanup(z, f, w, pol, res, zer) {
  const negligible = [];
  for (let i = 0; i < res.length; i++) {
    if (cabs(res[i]) < 1e-13) negligible.push(i);
  }
  if (negligible.length === 0) return { z, f, w, pol, res, zer };
  const remove = /* @__PURE__ */ new Set();
  for (const ni of negligible) {
    let minDist = Infinity, nearest = -1;
    for (let k = 0; k < z.length; k++) {
      if (remove.has(k)) continue;
      const dr = z[k][0] - pol[ni][0], di = z[k][1] - pol[ni][1];
      const dist = Math.sqrt(dr * dr + di * di);
      if (dist < minDist) {
        minDist = dist;
        nearest = k;
      }
    }
    if (nearest >= 0) remove.add(nearest);
  }
  const newZ = [], newF = [], newW = [];
  for (let i = 0; i < z.length; i++) {
    if (!remove.has(i)) {
      newZ.push(z[i]);
      newF.push(f[i]);
      newW.push(w[i]);
    }
  }
  const result = prz(newZ, newF, newW);
  return { z: newZ, f: newF, w: newW, pol: result.pol, res: result.res, zer: result.zer };
}

// notebooks/laplace/aaa-entry.js
var import_ndarray = __toESM(require_ndarray(), 1);
var timings = {
  svd: 0,
  cauchy: 0,
  aMatrix: 0,
  ratEval: 0,
  prz: 0,
  total: 0,
  iters: 0,
  reset() {
    this.svd = 0;
    this.cauchy = 0;
    this.aMatrix = 0;
    this.ratEval = 0;
    this.prz = 0;
    this.total = 0;
    this.iters = 0;
  }
};
function aaa(Z, F, tol = 1e-13, mmax = 100) {
  const t_total = performance.now();
  const M = Z.length;
  if (M !== F.length) throw new Error("Z and F must have the same length");
  if (mmax > M) mmax = M;
  const Zr = new Float64Array(2 * M);
  const Fr = new Float64Array(2 * M);
  for (let i = 0; i < M; i++) {
    Zr[2 * i] = Z[i][0];
    Zr[2 * i + 1] = Z[i][1];
    Fr[2 * i] = F[i][0];
    Fr[2 * i + 1] = F[i][1];
  }
  const zr = new Float64Array(2 * mmax);
  const fr = new Float64Array(2 * mmax);
  const wr = new Float64Array(2 * mmax);
  const errvec = [];
  const Cr = new Float64Array(2 * M * mmax);
  const J = new Int32Array(M);
  for (let i = 0; i < M; i++) J[i] = i;
  let nJ = M;
  const Rr = new Float64Array(2 * M);
  let meanR = 0, meanI = 0;
  for (let i = 0; i < M; i++) {
    meanR += Fr[2 * i];
    meanI += Fr[2 * i + 1];
  }
  meanR /= M;
  meanI /= M;
  for (let i = 0; i < M; i++) {
    Rr[2 * i] = meanR;
    Rr[2 * i + 1] = meanI;
  }
  let Fnorm = 0;
  for (let i = 0; i < M; i++) {
    const a = Math.hypot(Fr[2 * i], Fr[2 * i + 1]);
    if (a > Fnorm) Fnorm = a;
  }
  const maxN = mmax;
  const maxM_svd = M;
  const Adata = new import_complex128.default(maxM_svd * maxN);
  const Av = (0, import_reinterpret_complex128.default)(Adata, 0);
  const sData = new Float64Array(maxN);
  const UData = new import_complex128.default(1);
  const VTData = new import_complex128.default(maxN * maxN);
  const VTv = (0, import_reinterpret_complex128.default)(VTData, 0);
  const minMN = Math.min(maxM_svd, maxN);
  const maxMN = Math.max(maxM_svd, maxN);
  const lworkMax = Math.max(1, 3 * minMN + maxMN + minMN * maxMN);
  const WORK = new import_complex128.default(lworkMax);
  const RWORK = new Float64Array(5 * maxN);
  let converged = false;
  let m = 0;
  for (let iter = 0; iter < mmax; iter++) {
    timings.iters++;
    let maxVal = -1, j = 0;
    for (let i = 0; i < M; i++) {
      const dr = Fr[2 * i] - Rr[2 * i], di = Fr[2 * i + 1] - Rr[2 * i + 1];
      const a = dr * dr + di * di;
      if (a > maxVal) {
        maxVal = a;
        j = i;
      }
    }
    zr[2 * m] = Zr[2 * j];
    zr[2 * m + 1] = Zr[2 * j + 1];
    fr[2 * m] = Fr[2 * j];
    fr[2 * m + 1] = Fr[2 * j + 1];
    m++;
    for (let i = 0; i < nJ; i++) {
      if (J[i] === j) {
        for (let k = i; k < nJ - 1; k++) J[k] = J[k + 1];
        nJ--;
        break;
      }
    }
    let t0 = performance.now();
    const zjr = Zr[2 * j], zji = Zr[2 * j + 1];
    for (let i = 0; i < M; i++) {
      const dr = Zr[2 * i] - zjr, di = Zr[2 * i + 1] - zji;
      const d = dr * dr + di * di;
      const base = 2 * (i + iter * M);
      if (d === 0) {
        Cr[base] = 0;
        Cr[base + 1] = 0;
      } else {
        Cr[base] = dr / d;
        Cr[base + 1] = -di / d;
      }
    }
    timings.cauchy += performance.now() - t0;
    t0 = performance.now();
    for (let k = 0; k < m; k++) {
      const fkr = fr[2 * k], fki = fr[2 * k + 1];
      for (let ii = 0; ii < nJ; ii++) {
        const ci = J[ii];
        const cBase = 2 * (ci + k * M);
        const cr = Cr[cBase], cim = Cr[cBase + 1];
        const dr = Fr[2 * ci] - fkr, di = Fr[2 * ci + 1] - fki;
        const idx = 2 * (ii + k * nJ);
        Av[idx] = cr * dr - cim * di;
        Av[idx + 1] = cr * di + cim * dr;
      }
    }
    timings.aMatrix += performance.now() - t0;
    const lwork = Math.max(1, 8 * (nJ + m));
    for (let i = 0; i < 2 * m * m; i++) VTv[i] = 0;
    t0 = performance.now();
    const info = (0, import_ndarray.default)(
      "N",
      "A",
      nJ,
      m,
      Adata,
      1,
      nJ,
      0,
      sData,
      1,
      0,
      UData,
      1,
      1,
      0,
      VTData,
      1,
      m,
      0,
      WORK,
      1,
      0,
      lwork,
      RWORK,
      1,
      0
    );
    timings.svd += performance.now() - t0;
    if (info !== 0) console.warn("zgesvd info =", info);
    const lastRow = m - 1;
    for (let jj = 0; jj < m; jj++) {
      const idx = 2 * (lastRow + jj * m);
      wr[2 * jj] = VTv[idx];
      wr[2 * jj + 1] = -VTv[idx + 1];
    }
    t0 = performance.now();
    for (let i = 0; i < 2 * M; i++) Rr[i] = Fr[i];
    for (let ii = 0; ii < nJ; ii++) {
      const ci = J[ii];
      let nr = 0, ni = 0, dr = 0, di = 0;
      for (let k = 0; k < m; k++) {
        const cBase = 2 * (ci + k * M);
        const cr = Cr[cBase], cim = Cr[cBase + 1];
        const wkr = wr[2 * k], wki = wr[2 * k + 1];
        const fkr = fr[2 * k], fki = fr[2 * k + 1];
        const wfr = wkr * fkr - wki * fki;
        const wfi = wkr * fki + wki * fkr;
        nr += cr * wfr - cim * wfi;
        ni += cr * wfi + cim * wfr;
        dr += cr * wkr - cim * wki;
        di += cr * wki + cim * wkr;
      }
      const d = dr * dr + di * di;
      Rr[2 * ci] = (nr * dr + ni * di) / d;
      Rr[2 * ci + 1] = (ni * dr - nr * di) / d;
    }
    timings.ratEval += performance.now() - t0;
    let err = 0;
    for (let i = 0; i < M; i++) {
      const a = Math.hypot(Fr[2 * i] - Rr[2 * i], Fr[2 * i + 1] - Rr[2 * i + 1]);
      if (a > err) err = a;
    }
    errvec.push(err);
    if (err <= tol * Fnorm) {
      converged = true;
      break;
    }
  }
  const zOut = new Array(m);
  const fOut = new Array(m);
  const wOut = new Array(m);
  for (let i = 0; i < m; i++) {
    zOut[i] = [zr[2 * i], zr[2 * i + 1]];
    fOut[i] = [fr[2 * i], fr[2 * i + 1]];
    wOut[i] = [wr[2 * i], wr[2 * i + 1]];
  }
  const t_prz = performance.now();
  const { pol, res, zer } = prz(zOut, fOut, wOut);
  timings.prz += performance.now() - t_prz;
  timings.total += performance.now() - t_total;
  return {
    converged,
    z: zOut,
    f: fOut,
    w: wOut,
    errvec,
    pol,
    res,
    zer,
    eval: (zv) => feval([zv], zOut, fOut, wOut)[0],
    delete: () => {
    }
  };
}
export {
  aaa,
  barycentricRoots,
  cleanup,
  feval,
  prz,
  timings
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
/**
* @license Apache-2.0
*
* Complex number arithmetic utilities.
*
* Two categories of functions:
*
* 1. **Scalar operations** (re-exported from stdlib): operate on Complex128
*    objects. Use these for occasional scalar computations outside hot loops.
*
* 2. **Indexed operations**: operate directly on a Float64Array view at a
*    given index. Use these inside hot inner loops to avoid allocating
*    Complex128 objects on every iteration.
*
* Usage (scalar):
*   var Complex128 = require('@stdlib/complex/float64/ctor');
*   var z1 = new Complex128(3, 4);
*   var z2 = new Complex128(1, 2);
*   var z3 = cmplx.mul(z1, z2);  // returns Complex128
*   var r = cmplx.abs(z1);        // returns number
*
* Usage (indexed, for hot loops on Float64Array views):
*   var reinterpret = require('@stdlib/strided/base/reinterpret-complex128');
*   var view = reinterpret(complexArray, 0);
*   var r = cmplx.absAt(view, idx);                     // |view[idx] + view[idx+1]*i|
*   cmplx.mulAt(view, outIdx, view, aIdx, view, bIdx);  // view[out] = view[a] * view[b]
*/
/*! Bundled license information:

@stdlib/utils/define-property/lib/define_property.js:
@stdlib/utils/define-property/lib/has_define_property_support.js:
@stdlib/regexp/function-name/lib/main.js:
@stdlib/regexp/function-name/lib/index.js:
@stdlib/strided/base/reinterpret-complex64/lib/main.js:
@stdlib/strided/base/reinterpret-complex64/lib/index.js:
@stdlib/strided/base/reinterpret-complex128/lib/main.js:
@stdlib/strided/base/reinterpret-complex128/lib/index.js:
@stdlib/math/base/special/abs/lib/main.js:
  (**
  * @license Apache-2.0
  *
  * Copyright (c) 2021 The Stdlib Authors.
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

@stdlib/utils/define-property/lib/builtin.js:
@stdlib/utils/define-property/lib/polyfill.js:
@stdlib/utils/define-property/lib/index.js:
@stdlib/utils/define-nonenumerable-read-only-property/lib/main.js:
@stdlib/utils/define-nonenumerable-read-only-property/lib/index.js:
@stdlib/assert/is-number/lib/primitive.js:
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
@stdlib/number/ctor/lib/main.js:
@stdlib/number/ctor/lib/index.js:
@stdlib/assert/is-number/lib/tostring.js:
@stdlib/assert/is-number/lib/try2serialize.js:
@stdlib/assert/is-number/lib/object.js:
@stdlib/assert/is-number/lib/main.js:
@stdlib/assert/is-number/lib/index.js:
@stdlib/constants/float64/pinf/lib/index.js:
@stdlib/constants/float64/ninf/lib/index.js:
@stdlib/math/base/special/floor/lib/main.js:
@stdlib/math/base/special/floor/lib/index.js:
@stdlib/math/base/assert/is-integer/lib/main.js:
@stdlib/math/base/assert/is-integer/lib/index.js:
@stdlib/assert/is-integer/lib/integer.js:
@stdlib/assert/is-integer/lib/primitive.js:
@stdlib/assert/is-integer/lib/object.js:
@stdlib/assert/is-integer/lib/main.js:
@stdlib/assert/is-integer/lib/index.js:
@stdlib/assert/is-nonnegative-integer/lib/primitive.js:
@stdlib/assert/is-nonnegative-integer/lib/object.js:
@stdlib/assert/is-nonnegative-integer/lib/main.js:
@stdlib/assert/is-nonnegative-integer/lib/index.js:
@stdlib/constants/array/max-array-length/lib/index.js:
@stdlib/assert/is-array-like-object/lib/main.js:
@stdlib/assert/is-array-like-object/lib/index.js:
@stdlib/constants/array/max-typed-array-length/lib/index.js:
@stdlib/assert/is-collection/lib/main.js:
@stdlib/assert/is-collection/lib/index.js:
@stdlib/assert/is-arraybuffer/lib/main.js:
@stdlib/assert/is-arraybuffer/lib/index.js:
@stdlib/assert/is-array/lib/main.js:
@stdlib/assert/is-array/lib/index.js:
@stdlib/assert/is-object/lib/main.js:
@stdlib/assert/is-object/lib/index.js:
@stdlib/assert/tools/array-function/lib/main.js:
@stdlib/assert/tools/array-function/lib/index.js:
@stdlib/assert/is-string/lib/primitive.js:
@stdlib/assert/is-string/lib/valueof.js:
@stdlib/assert/is-string/lib/try2valueof.js:
@stdlib/assert/is-string/lib/object.js:
@stdlib/assert/is-string/lib/main.js:
@stdlib/assert/is-string/lib/index.js:
@stdlib/assert/is-string-array/lib/index.js:
@stdlib/utils/type-of/lib/fixtures/re.js:
@stdlib/assert/is-boolean/lib/primitive.js:
@stdlib/assert/is-boolean/lib/tostring.js:
@stdlib/assert/is-boolean/lib/try2serialize.js:
@stdlib/assert/is-boolean/lib/object.js:
@stdlib/assert/is-boolean/lib/main.js:
@stdlib/assert/is-boolean/lib/index.js:
@stdlib/utils/global/lib/codegen.js:
@stdlib/utils/global/lib/self.js:
@stdlib/utils/global/lib/window.js:
@stdlib/utils/global/lib/global.js:
@stdlib/utils/global/lib/main.js:
@stdlib/utils/global/lib/index.js:
@stdlib/utils/type-of/lib/fixtures/nodelist.js:
@stdlib/utils/type-of/lib/fixtures/typedarray.js:
@stdlib/utils/type-of/lib/check.js:
@stdlib/regexp/function-name/lib/regexp.js:
@stdlib/assert/is-object-like/lib/main.js:
@stdlib/assert/is-object-like/lib/index.js:
@stdlib/assert/is-buffer/lib/main.js:
@stdlib/assert/is-buffer/lib/index.js:
@stdlib/utils/constructor-name/lib/main.js:
@stdlib/utils/constructor-name/lib/index.js:
@stdlib/utils/type-of/lib/main.js:
@stdlib/utils/type-of/lib/polyfill.js:
@stdlib/utils/type-of/lib/index.js:
@stdlib/assert/is-function/lib/main.js:
@stdlib/assert/is-function/lib/index.js:
@stdlib/complex/float64/ctor/lib/tostring.js:
@stdlib/complex/float64/ctor/lib/tojson.js:
@stdlib/complex/float64/ctor/lib/main.js:
@stdlib/complex/float64/ctor/lib/index.js:
@stdlib/number/float64/base/to-float32/lib/main.js:
@stdlib/assert/is-float32array/lib/main.js:
@stdlib/assert/is-float32array/lib/index.js:
@stdlib/assert/has-float32array-support/lib/float32array.js:
@stdlib/assert/has-float32array-support/lib/main.js:
@stdlib/assert/has-float32array-support/lib/index.js:
@stdlib/array/float32/lib/main.js:
@stdlib/array/float32/lib/polyfill.js:
@stdlib/array/float32/lib/index.js:
@stdlib/number/float64/base/to-float32/lib/polyfill.js:
@stdlib/number/float64/base/to-float32/lib/index.js:
@stdlib/complex/float32/ctor/lib/tostring.js:
@stdlib/complex/float32/ctor/lib/tojson.js:
@stdlib/complex/float32/ctor/lib/main.js:
@stdlib/complex/float32/ctor/lib/index.js:
@stdlib/assert/is-complex-like/lib/main.js:
@stdlib/assert/is-complex-like/lib/index.js:
@stdlib/math/base/assert/is-even/lib/main.js:
@stdlib/math/base/assert/is-even/lib/index.js:
@stdlib/assert/has-iterator-symbol-support/lib/main.js:
@stdlib/assert/has-iterator-symbol-support/lib/index.js:
@stdlib/symbol/iterator/lib/main.js:
@stdlib/symbol/iterator/lib/index.js:
@stdlib/utils/define-nonenumerable-read-only-accessor/lib/main.js:
@stdlib/utils/define-nonenumerable-read-only-accessor/lib/index.js:
@stdlib/assert/is-float64array/lib/main.js:
@stdlib/assert/is-float64array/lib/index.js:
@stdlib/assert/has-float64array-support/lib/float64array.js:
@stdlib/assert/has-float64array-support/lib/main.js:
@stdlib/assert/has-float64array-support/lib/index.js:
@stdlib/array/float64/lib/main.js:
@stdlib/array/float64/lib/polyfill.js:
@stdlib/array/float64/lib/index.js:
@stdlib/complex/float64/real/lib/main.js:
@stdlib/complex/float64/real/lib/index.js:
@stdlib/complex/float64/imag/lib/main.js:
@stdlib/complex/float64/imag/lib/index.js:
@stdlib/array/complex128/lib/from_iterator.js:
@stdlib/array/complex128/lib/from_iterator_map.js:
@stdlib/array/complex128/lib/from_array.js:
@stdlib/array/complex128/lib/index.js:
@stdlib/complex/float64/base/mul/lib/main.js:
@stdlib/complex/float64/base/mul/lib/index.js:
@stdlib/complex/float64/base/add/lib/main.js:
@stdlib/complex/float64/base/add/lib/index.js:
@stdlib/math/base/special/abs/lib/index.js:
@stdlib/math/base/assert/is-positive-zero/lib/main.js:
@stdlib/math/base/assert/is-positive-zero/lib/index.js:
@stdlib/math/base/assert/is-nan/lib/main.js:
@stdlib/math/base/assert/is-nan/lib/index.js:
@stdlib/math/base/special/max/lib/main.js:
@stdlib/math/base/special/max/lib/index.js:
@stdlib/constants/float64/max/lib/index.js:
@stdlib/constants/float64/smallest-normal/lib/index.js:
@stdlib/constants/float64/eps/lib/index.js:
@stdlib/math/base/ops/cdiv/lib/internal_compreal.js:
@stdlib/math/base/ops/cdiv/lib/robust_internal.js:
@stdlib/math/base/ops/cdiv/lib/main.js:
@stdlib/math/base/ops/cdiv/lib/index.js:
@stdlib/math/base/ops/csub/lib/main.js:
@stdlib/math/base/ops/csub/lib/index.js:
@stdlib/math/base/ops/cneg/lib/main.js:
@stdlib/math/base/ops/cneg/lib/index.js:
@stdlib/math/base/assert/is-infinite/lib/main.js:
@stdlib/math/base/assert/is-infinite/lib/index.js:
@stdlib/math/base/special/sqrt/lib/main.js:
@stdlib/math/base/special/sqrt/lib/index.js:
@stdlib/math/base/special/hypot/lib/main.js:
@stdlib/math/base/special/hypot/lib/index.js:
@stdlib/math/base/special/cabs/lib/main.js:
@stdlib/math/base/special/cabs/lib/index.js:
@stdlib/math/base/special/cabs2/lib/main.js:
@stdlib/math/base/special/cabs2/lib/index.js:
@stdlib/complex/float64/conj/lib/main.js:
@stdlib/complex/float64/conj/lib/index.js:
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

@stdlib/string/base/format-interpolate/lib/is_number.js:
@stdlib/string/base/format-interpolate/lib/zero_pad.js:
@stdlib/string/base/format-interpolate/lib/format_integer.js:
@stdlib/string/base/format-interpolate/lib/is_string.js:
@stdlib/string/base/format-interpolate/lib/format_double.js:
@stdlib/string/base/format-interpolate/lib/space_pad.js:
@stdlib/string/base/format-interpolate/lib/main.js:
@stdlib/string/base/format-interpolate/lib/index.js:
@stdlib/string/base/format-tokenize/lib/main.js:
@stdlib/string/base/format-tokenize/lib/index.js:
@stdlib/string/format/lib/is_string.js:
@stdlib/string/format/lib/main.js:
@stdlib/string/format/lib/index.js:
@stdlib/boolean/ctor/lib/main.js:
@stdlib/boolean/ctor/lib/index.js:
@stdlib/utils/global/lib/global_this.js:
@stdlib/array/base/getter/lib/main.js:
@stdlib/array/base/getter/lib/index.js:
@stdlib/array/base/accessor-getter/lib/main.js:
@stdlib/array/base/accessor-getter/lib/index.js:
  (**
  * @license Apache-2.0
  *
  * Copyright (c) 2022 The Stdlib Authors.
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

@stdlib/array/base/assert/is-complex64array/lib/main.js:
@stdlib/array/base/assert/is-complex64array/lib/index.js:
@stdlib/array/base/assert/is-complex128array/lib/main.js:
@stdlib/array/base/assert/is-complex128array/lib/index.js:
@stdlib/array/complex128/lib/main.js:
  (**
  * @license Apache-2.0
  *
  * Copyright (c) 2024 The Stdlib Authors.
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
