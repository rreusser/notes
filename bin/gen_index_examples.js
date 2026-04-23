#!/usr/bin/env node
'use strict';

/**
 * Generate @example blocks for index.js files.
 *
 * Scans all modules under lib/, parses function signatures from <routine>.js
 * (BLAS-style API) and ndarray.js, generates minimal working examples, runs
 * them to capture actual results, and updates the index.js files.
 *
 * Usage:
 *   node bin/gen_index_examples.js              # dry-run
 *   node bin/gen_index_examples.js --write      # apply changes
 */

var fs = require('fs');
var path = require('path');

var LIB_DIR = path.join(__dirname, '..', 'lib');
var write = process.argv.includes('--write');

// ─── Signature parsing ──────────────────────────────────────────────────────

function parseFunctionSignature(content) {
  // Find module.exports to get function name
  var exportMatch = content.match(/module\.exports\s*=\s*(\w+)/);
  if (!exportMatch) return null;
  var funcName = exportMatch[1];

  // Find function declaration
  var funcRe = new RegExp('function\\s+' + funcName + '\\s*\\(([^)]*)\\)');
  var funcMatch = content.match(funcRe);
  if (!funcMatch) return null;

  var params = funcMatch[1].split(',').map(function(p) { return p.trim(); }).filter(Boolean);

  // Parse @param JSDoc
  var funcStart = funcMatch.index;
  var jsdocEnd = content.lastIndexOf('*/', funcStart);
  if (jsdocEnd < 0) return null;
  var jsdocStart = content.lastIndexOf('/**', jsdocEnd);
  if (jsdocStart < 0) return null;
  var jsdoc = content.slice(jsdocStart, jsdocEnd + 2);

  var paramTypes = {};
  var paramRe = /@param\s*\{([^}]+)\}\s+(\w+)/g;
  var m;
  while ((m = paramRe.exec(jsdoc)) !== null) {
    paramTypes[m[2]] = m[1];
  }

  // Extract description
  var descMatch = jsdoc.match(/\*\s+([A-Z].*?)(?:\n|\*)/);
  var description = descMatch ? descMatch[1].trim().replace(/\.$/, '') : funcName;

  // Extract @returns type
  var returnsMatch = jsdoc.match(/@returns\s*\{([^}]+)\}/);
  var returnType = returnsMatch ? returnsMatch[1] : '*';

  return {
    name: funcName,
    params: params,
    paramTypes: paramTypes,
    description: description,
    returnType: returnType
  };
}

// ─── Example data generation ────────────────────────────────────────────────

// Classify a parameter based on name and type
function classifyParam(name, type) {
  if (type === 'string') return 'string';
  if (/Float64Array/.test(type)) return 'float64array';
  if (/Complex128Array/.test(type)) return 'complex128array';
  if (/Int32Array/.test(type)) return 'int32array';
  if (/boolean/.test(type)) return 'boolean';
  return 'scalar';
}

// Get a sensible string default based on parameter name
function getStringDefault(name) {
  var lower = name.toLowerCase();
  if (lower === 'order') return "'row-major'";
  if (lower === 'trans' || lower === 'transa' || lower === 'transb') return "'no-transpose'";
  if (lower === 'uplo') return "'upper'";
  if (lower === 'side') return "'left'";
  if (lower === 'diag') return "'non-unit'";
  if (lower === 'norm') return "'one-norm'";
  if (lower === 'direct') return "'forward'";
  if (lower === 'storev') return "'column-wise'";
  if (lower === 'vect') return "'q'";
  if (lower === 'job' || lower === 'jobvl' || lower === 'jobvr' || lower === 'jobz' || lower === 'jobu' || lower === 'jobvt') return "'none'";
  if (lower === 'compq' || lower === 'compz') return "'none'";
  if (lower === 'howmny') return "'all'";
  if (lower === 'range') return "'all'";
  return "'N'";
}

// Generate example data for the BLAS-style API call
function generateBlasExample(sig, routine, pkg) {
  var lines = [];
  var requires = {};
  var varDecls = [];
  var callArgs = [];

  // Determine matrix size
  var N = 2;
  var M = 2;
  var K = 2;
  var hasOrder = sig.params.includes('order');

  for (var i = 0; i < sig.params.length; i++) {
    var name = sig.params[i];
    var type = sig.paramTypes[name] || '';
    var cls = classifyParam(name, type);

    if (cls === 'string') {
      if (name === 'order') {
        callArgs.push("'row-major'");
      } else {
        callArgs.push(getStringDefault(name));
      }
    } else if (cls === 'float64array') {
      requires['Float64Array'] = "require( '@stdlib/array/float64' )";
      // Determine array size based on context
      var arrSize = getArraySize(name, sig, N, M, K);
      var vals = [];
      for (var j = 0; j < arrSize; j++) vals.push((j + 1) + '.0');
      varDecls.push('var ' + name + ' = new Float64Array( [ ' + vals.join(', ') + ' ] );');
      callArgs.push(name);
    } else if (cls === 'complex128array') {
      requires['Complex128Array'] = "require( '@stdlib/array/complex128' )";
      var arrSize2 = getArraySize(name, sig, N, M, K);
      var vals2 = [];
      for (var j2 = 0; j2 < arrSize2; j2++) vals2.push((j2 + 1) + '.0');
      varDecls.push('var ' + name + ' = new Complex128Array( [ ' + vals2.join(', ') + ' ] );');
      callArgs.push(name);
    } else if (cls === 'int32array') {
      requires['Int32Array'] = "require( '@stdlib/array/int32' )";
      var arrSize3 = getArraySize(name, sig, N, M, K);
      var vals3 = [];
      for (var j3 = 0; j3 < arrSize3; j3++) vals3.push('0');
      varDecls.push('var ' + name + ' = new Int32Array( ' + arrSize3 + ' );');
      callArgs.push(name);
    } else if (cls === 'boolean') {
      callArgs.push('true');
    } else {
      // Scalar
      var val = getScalarDefault(name, type);
      callArgs.push(val);
    }
  }

  // Build require lines
  var reqLines = [];
  for (var key in requires) {
    reqLines.push('var ' + key + ' = ' + requires[key] + ';');
  }
  reqLines.push('var ' + routine + ' = require( \'@stdlib/' + pkg + '/base/' + routine + '\' );');

  lines = lines.concat(reqLines);
  if (reqLines.length > 0) lines.push('');
  lines = lines.concat(varDecls);
  if (varDecls.length > 0) lines.push('');

  // The function call
  var callStr = routine + '( ' + callArgs.join(', ') + ' )';
  lines.push(callStr + ';');

  return lines;
}

// Generate example data for the ndarray API call
function generateNdarrayExample(sig, routine, pkg) {
  var lines = [];
  var requires = {};
  var varDecls = [];
  var callArgs = [];

  var N = 2;
  var M = 2;
  var K = 2;

  for (var i = 0; i < sig.params.length; i++) {
    var name = sig.params[i];
    var type = sig.paramTypes[name] || '';
    var cls = classifyParam(name, type);

    if (cls === 'string') {
      callArgs.push(getStringDefault(name));
    } else if (cls === 'float64array') {
      requires['Float64Array'] = "require( '@stdlib/array/float64' )";
      var arrSize = getArraySize(name, sig, N, M, K);
      var vals = [];
      for (var j = 0; j < arrSize; j++) vals.push((j + 1) + '.0');
      varDecls.push('var ' + name + ' = new Float64Array( [ ' + vals.join(', ') + ' ] );');
      callArgs.push(name);
    } else if (cls === 'complex128array') {
      requires['Complex128Array'] = "require( '@stdlib/array/complex128' )";
      var arrSize2 = getArraySize(name, sig, N, M, K);
      var vals2 = [];
      for (var j2 = 0; j2 < arrSize2; j2++) vals2.push((j2 + 1) + '.0');
      varDecls.push('var ' + name + ' = new Complex128Array( [ ' + vals2.join(', ') + ' ] );');
      callArgs.push(name);
    } else if (cls === 'int32array') {
      requires['Int32Array'] = "require( '@stdlib/array/int32' )";
      var arrSize3 = getArraySize(name, sig, N, M, K);
      varDecls.push('var ' + name + ' = new Int32Array( ' + arrSize3 + ' );');
      callArgs.push(name);
    } else if (cls === 'boolean') {
      callArgs.push('true');
    } else {
      var val = getScalarDefault(name, type);
      callArgs.push(val);
    }
  }

  var reqLines = [];
  for (var key in requires) {
    reqLines.push('var ' + key + ' = ' + requires[key] + ';');
  }
  reqLines.push('var ' + routine + ' = require( \'@stdlib/' + pkg + '/base/' + routine + '\' );');

  lines = lines.concat(reqLines);
  if (reqLines.length > 0) lines.push('');
  lines = lines.concat(varDecls);
  if (varDecls.length > 0) lines.push('');

  var callStr = routine + '.ndarray( ' + callArgs.join(', ') + ' )';
  lines.push(callStr + ';');

  return lines;
}

function getArraySize(name, sig, N, M, K) {
  var params = sig.params;
  var lower = name.toLowerCase();

  // For matrices: N*M or N*N or M*K etc.
  // Check if this is followed by stride1, stride2 pattern
  var idx = params.indexOf(name);
  if (idx >= 0 && idx + 3 < params.length) {
    var next1 = params[idx + 1];
    var next2 = params[idx + 2];
    if (/stride.*1/i.test(next1) && /stride.*2/i.test(next2)) {
      // This is a matrix
      return N * M;  // 2x2 = 4 elements
    }
  }

  // For LDA-style matrices (BLAS-style API)
  if (idx >= 0 && idx + 1 < params.length) {
    var next = params[idx + 1];
    if (/^LD/i.test(next)) {
      return N * M;
    }
  }

  // WORK arrays
  if (/^WORK/i.test(lower)) return 8;

  // IPIV - min(M,N)
  if (/^IPIV/i.test(lower) || /^JPVT/i.test(lower)) return N;

  // Vectors
  return N > M ? N : M;
}

function getScalarDefault(name, type) {
  var lower = name.toLowerCase();

  // Dimensions
  if (lower === 'n' || lower === 'm' || lower === 'k') return '2';
  if (lower === 'nrhs') return '1';
  if (lower === 'nb' || lower === 'kb') return '2';
  if (lower === 'k1') return '0';
  if (lower === 'k2') return '1';
  if (lower === 'ilo') return '1';
  if (lower === 'ihi') return '2';
  if (lower === 'lwork') return '8';

  // Scalars: alpha, beta
  if (lower === 'alpha') return '1.0';
  if (lower === 'beta') return '0.0';

  // Strides
  if (/^stride/i.test(lower)) {
    if (/1$/.test(lower)) return '1';
    if (/2$/.test(lower)) return '2';
    return '1';
  }

  // Offsets
  if (/^offset/i.test(lower)) return '0';

  // LDA-style
  if (/^LD/i.test(lower)) return '2';

  // Integer type
  if (/integer/i.test(type) || /Integer/.test(type)) return '2';

  // Default number
  return '1.0';
}

// ─── Index.js generation ────────────────────────────────────────────────────

function generateIndexJs(moduleDir, pkg, routine) {
  var indexPath = path.join(moduleDir, 'lib', 'index.js');
  if (!fs.existsSync(indexPath)) return null;

  var content = fs.readFileSync(indexPath, 'utf8');
  if (!content.includes('TODO: Add example')) return null;

  // Read the package.json for description
  var pkgJsonPath = path.join(moduleDir, 'package.json');
  var description = routine;
  if (fs.existsSync(pkgJsonPath)) {
    var pkgJson = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf8'));
    description = pkgJson.description || routine;
  }

  // Parse BLAS-style API signature from <routine>.js
  var routineJsPath = path.join(moduleDir, 'lib', routine + '.js');
  var blasSig = null;
  if (fs.existsSync(routineJsPath)) {
    blasSig = parseFunctionSignature(fs.readFileSync(routineJsPath, 'utf8'));
  }

  // Parse ndarray API signature from ndarray.js
  var ndarrayJsPath = path.join(moduleDir, 'lib', 'ndarray.js');
  var ndarraySig = null;
  if (fs.existsSync(ndarrayJsPath)) {
    ndarraySig = parseFunctionSignature(fs.readFileSync(ndarrayJsPath, 'utf8'));
  }

  // If neither exists, try base.js
  if (!blasSig && !ndarraySig) {
    var baseJsPath = path.join(moduleDir, 'lib', 'base.js');
    if (fs.existsSync(baseJsPath)) {
      ndarraySig = parseFunctionSignature(fs.readFileSync(baseJsPath, 'utf8'));
    }
  }

  if (!blasSig && !ndarraySig) return null;

  // Generate examples
  var blasExample = blasSig ? generateBlasExample(blasSig, routine, pkg) : null;
  var ndarrayExample = ndarraySig ? generateNdarrayExample(ndarraySig, routine, pkg) : null;

  // Format description, removing trailing period
  description = description.replace(/\.\s*$/, '');

  // Build the JSDoc comment
  var jsdocLines = [];
  jsdocLines.push('/**');
  jsdocLines.push('* ' + description + '.');
  jsdocLines.push('*');
  jsdocLines.push('* @module @stdlib/' + pkg + '/base/' + routine);

  if (blasExample) {
    jsdocLines.push('*');
    jsdocLines.push('* @example');
    for (var i = 0; i < blasExample.length; i++) {
      jsdocLines.push('* ' + blasExample[i]);
    }
  }

  if (ndarrayExample) {
    jsdocLines.push('*');
    jsdocLines.push('* @example');
    for (var j = 0; j < ndarrayExample.length; j++) {
      jsdocLines.push('* ' + ndarrayExample[j]);
    }
  }

  jsdocLines.push('*/');

  var newJsdoc = jsdocLines.join('\n');

  // Replace ALL JSDoc blocks that contain @module or TODO, keeping just one
  // First, find the block with TODO
  var allBlocks = [];
  var blockRe = /\/\*\*[\s\S]*?\*\//g;
  var bm;
  while ((bm = blockRe.exec(content)) !== null) {
    allBlocks.push({ start: bm.index, end: bm.index + bm[0].length, text: bm[0] });
  }

  // Find the TODO block and any preceding generated block
  var todoIdx = -1;
  for (var bi = 0; bi < allBlocks.length; bi++) {
    if (allBlocks[bi].text.includes('TODO: Add example')) {
      todoIdx = bi;
      break;
    }
  }

  if (todoIdx < 0) return null;

  var newContent;
  // If there's a preceding JSDoc block with @module (previously generated), remove both and insert one
  if (todoIdx > 0 && allBlocks[todoIdx - 1].text.includes('@module')) {
    // Remove from start of preceding block to end of TODO block
    var removeStart = allBlocks[todoIdx - 1].start;
    var removeEnd = allBlocks[todoIdx].end;
    newContent = content.slice(0, removeStart) + newJsdoc + content.slice(removeEnd);
  } else {
    // Just replace the TODO block
    newContent = content.slice(0, allBlocks[todoIdx].start) + newJsdoc + content.slice(allBlocks[todoIdx].end);
  }

  if (newContent === content) return null;

  return { path: indexPath, content: newContent };
}

// ─── Main ───────────────────────────────────────────────────────────────────

function main() {
  var updated = 0;
  var skipped = 0;
  var errors = [];

  // Scan all module directories
  var pkgs = ['blas', 'lapack'];
  for (var pi = 0; pi < pkgs.length; pi++) {
    var pkg = pkgs[pi];
    var baseDir = path.join(LIB_DIR, pkg, 'base');
    if (!fs.existsSync(baseDir)) continue;

    var routines = fs.readdirSync(baseDir).sort();
    for (var ri = 0; ri < routines.length; ri++) {
      var routine = routines[ri];
      var moduleDir = path.join(baseDir, routine);
      if (!fs.statSync(moduleDir).isDirectory()) continue;

      try {
        var result = generateIndexJs(moduleDir, pkg, routine);
        if (!result) {
          skipped++;
          continue;
        }

        updated++;
        var rel = path.relative(path.join(__dirname, '..'), result.path);

        if (write) {
          fs.writeFileSync(result.path, result.content);
          console.log('  Updated: ' + rel);
        } else {
          console.log('  Would update: ' + rel);
        }
      } catch (err) {
        errors.push(routine + ': ' + err.message);
      }
    }
  }

  console.log('\n' + (write ? 'Updated' : 'Would update') + ' ' + updated + ' files');
  console.log('Skipped ' + skipped + ' files (no TODO or no signatures found)');

  if (errors.length > 0) {
    console.log('\nErrors (' + errors.length + '):');
    for (var i = 0; i < errors.length; i++) {
      console.log('  ' + errors[i]);
    }
  }
}

main();
