'use strict';

var fs = require( 'fs' );
var path = require( 'path' );

// String enum params and their default values
var STRING_PARAMS = {
	'order': '\'row-major\'',
	'trans': '\'no-transpose\'',
	'transa': '\'no-transpose\'',
	'transb': '\'no-transpose\'',
	'uplo': '\'upper\'',
	'side': '\'left\'',
	'diag': '\'non-unit\'',
	'job': '\'both\'',
	'jobvs': '\'V\'',
	'jobvl': '\'V\'',
	'jobvr': '\'V\'',
	'jobu': '\'A\'',
	'jobvt': '\'A\'',
	'jobz': '\'V\'',
	'joba': '\'E\'',
	'jobb': '\'B\'',
	'compq': '\'V\'',
	'compz': '\'V\'',
	'fact': '\'N\'',
	'equed': '\'N\'',
	'norm': '\'1\'',
	'range': '\'A\'',
	'sort': '\'N\'',
	'sense': '\'N\'',
	'direct': '\'forward\'',
	'storev': '\'columnwise\'',
	'vect': '\'Q\'',
	'howmny': '\'A\''
};

// Scalar params
var SCALAR_PARAMS = new Set([
	'alpha', 'beta', 'c', 's', 'da', 'anorm', 'scale', 'rcond',
	'dy1', 'vl', 'vu', 'abstol', 'tola', 'tolb', 'pivmin', 'smin',
	'ssfmin', 'ssfmax', 'sigma', 'mu', 'orgati', 'rho', 'dmin',
	'dmin1', 'dmin2', 'dn', 'dn1', 'dn2', 'g', 'tau', 'eps'
]);

// Integer params (dimensions, indices, etc.)
var INT_PARAMS = new Set([
	'N', 'M', 'K', 'kl', 'ku', 'nrhs', 'ilo', 'ihi',
	'ncvt', 'nru', 'ncc', 'lwork', 'sdim', 'rank', 'iseed',
	'itype', 'il', 'iu', 'i1', 'i2', 'nsplit', 'mm', 'iter',
	'ifst', 'ilst', 'LDA', 'LDB', 'LDC', 'LDQ', 'LDZ',
	'j1', 'n1', 'n2', 'kb'
]);

// Complex type prefixes
var COMPLEX_PREFIXES = new Set([ 'z', 'c' ]);

// Build a map of which params are arrays by scanning stride params.
// stride<Name> → 1D array, stride<Name>1 → 2D array
function buildArrayMap( params ) {
	var arrays1D = new Set();
	var arrays2D = new Set();
	var i, p, m, arrName;

	for ( i = 0; i < params.length; i++ ) {
		p = params[ i ];
		// Match stride<Name>1 or stride<Name>2 for 2D arrays
		m = p.match( /^stride(.+?)[12]$/ );
		if ( m ) {
			arrName = m[ 1 ];
			// Find the actual param name (may differ in case)
			var actual = findParam( params, arrName );
			if ( actual ) {
				arrays2D.add( actual );
			}
			continue;
		}
		// Match stride<Name> for 1D arrays (but not bare 'stride')
		m = p.match( /^stride(.+)$/ );
		if ( m && m[ 1 ] ) {
			arrName = m[ 1 ];
			if ( !arrays2D.has( arrName ) ) {
				var actual2 = findParam( params, arrName );
				if ( actual2 ) {
					arrays1D.add( actual2 );
				}
			}
		}
		// Also handle bare 'stride' — find the preceding non-classified param
		if ( p === 'stride' ) {
			// The array param is typically the one right before 'stride'
			if ( i > 0 ) {
				arrays1D.add( params[ i - 1 ] );
			}
		}
	}
	// Remove any that ended up in both
	arrays2D.forEach( function( a ) { arrays1D.delete( a ); });
	return { arrays1D: arrays1D, arrays2D: arrays2D };
}

// Find a param name matching arrName (case-insensitive first char)
function findParam( params, arrName ) {
	// Direct match
	if ( params.indexOf( arrName ) >= 0 ) {
		return arrName;
	}
	// Try lowercasing first char
	var lower = arrName[ 0 ].toLowerCase() + arrName.slice( 1 );
	if ( params.indexOf( lower ) >= 0 ) {
		return lower;
	}
	// Try uppercasing first char
	var upper = arrName[ 0 ].toUpperCase() + arrName.slice( 1 );
	if ( params.indexOf( upper ) >= 0 ) {
		return upper;
	}
	return null;
}

// Extract function signature from base.js
function extractSignature( filePath ) {
	var content = fs.readFileSync( filePath, 'utf8' );
	var m = content.match( /function\s+\w+\(\s*([^)]+)\)/ );
	if ( !m ) {
		return null;
	}
	return m[ 1 ].split( /,\s*/ ).map( function( s ) {
		return s.trim();
	});
}

// Extract layout wrapper signature
function extractLayoutSignature( filePath ) {
	if ( !fs.existsSync( filePath ) ) {
		return null;
	}
	return extractSignature( filePath );
}

// Classify params and determine what arrays/scalars to create
function classifyParams( params, isComplex ) {
	var arrayMap = buildArrayMap( params );
	var arrays2D = [];
	var arrays1D = [];
	var scalars = [];
	var stringEnums = [];
	var intParams = [];
	var strides = [];
	var offsets = [];
	var others = [];
	var selectFn = false;
	var lwork = false;
	var i, p, lower;

	// Convert sets to arrays preserving param order
	for ( i = 0; i < params.length; i++ ) {
		if ( arrayMap.arrays2D.has( params[ i ] ) ) {
			arrays2D.push( params[ i ] );
		}
		if ( arrayMap.arrays1D.has( params[ i ] ) ) {
			arrays1D.push( params[ i ] );
		}
	}

	for ( i = 0; i < params.length; i++ ) {
		p = params[ i ];
		lower = p.toLowerCase();

		if ( p.startsWith( 'stride' ) || p === 'stride' ) {
			strides.push( p );
		} else if ( p.startsWith( 'offset' ) || p === 'offset' ) {
			offsets.push( p );
		} else if ( arrayMap.arrays2D.has( p ) || arrayMap.arrays1D.has( p ) ) {
			// Already classified above
		} else if ( STRING_PARAMS[ lower ] ) {
			stringEnums.push({ name: p, value: STRING_PARAMS[ lower ] });
		} else if ( p === 'select' ) {
			selectFn = true;
		} else if ( lower === 'lwork' || lower === 'liwork' || lower === 'lrwork' ) {
			lwork = true;
		} else if ( SCALAR_PARAMS.has( lower ) ) {
			scalars.push( p );
		} else if ( INT_PARAMS.has( p ) ) {
			intParams.push( p );
		} else if ( lower === 'za' || lower === 'z' ) {
			scalars.push( p );
		} else {
			others.push( p );
		}
	}
	return {
		arrays2D: arrays2D,
		arrays1D: arrays1D,
		scalars: scalars,
		stringEnums: stringEnums,
		intParams: intParams,
		strides: strides,
		offsets: offsets,
		others: others,
		selectFn: selectFn,
		lwork: lwork
	};
}

// Generate the benchmark call arguments for ndarray (base.js) style
function genNdarrayArgs( params, classification ) {
	var args = [];
	var i, p, lower;

	for ( i = 0; i < params.length; i++ ) {
		p = params[ i ];
		lower = p.toLowerCase();

		if ( STRING_PARAMS[ lower ] ) {
			args.push( STRING_PARAMS[ lower ] );
		} else if ( p === 'select' ) {
			args.push( 'selectFcn' );
		} else if ( lower === 'lwork' ) {
			args.push( '-1' );
		} else if ( p === 'N' || p === 'M' || p === 'K' || p === 'nrhs' || p === 'ncvt' || p === 'nru' || p === 'ncc' || p === 'mm' ) {
			args.push( 'N' );
		} else if ( p === 'kl' || p === 'ku' || p === 'kb' ) {
			args.push( '1' );
		} else if ( p === 'ilo' || p === 'il' || p === 'ifst' || p === 'i1' ) {
			args.push( '1' );
		} else if ( p === 'ihi' || p === 'iu' || p === 'ilst' || p === 'i2' ) {
			args.push( 'N' );
		} else if ( p === 'itype' ) {
			args.push( '1' );
		} else if ( p === 'nsplit' ) {
			args.push( '1' );
		} else if ( p === 'j1' || p === 'n1' || p === 'n2' ) {
			args.push( '1' );
		} else if ( p === 'iter' ) {
			args.push( '0' );
		} else if ( p === 'sdim' || p === 'rank' ) {
			args.push( '0' );
		} else if ( classification.arrays2D.indexOf( p ) >= 0 || classification.arrays1D.indexOf( p ) >= 0 ) {
			args.push( p );
		} else if ( SCALAR_PARAMS.has( lower ) ) {
			args.push( '1.0' );
		} else if ( lower === 'za' || lower === 'z' ) {
			args.push( 'za' );
		} else if ( /^stride.+[12]$/.test( p ) ) {
			// 2D stride: stride<Name>1 or stride<Name>2
			if ( /1$/.test( p ) ) {
				args.push( 'N' );
			} else {
				args.push( '1' );
			}
		} else if ( p.startsWith( 'stride' ) || p === 'stride' ) {
			args.push( '1' );
		} else if ( p.startsWith( 'offset' ) || p === 'offset' ) {
			args.push( '0' );
		} else if ( INT_PARAMS.has( p ) ) {
			args.push( 'N' );
		} else {
			// Unclassified — treat as scalar with value 1
			args.push( '1' );
		}
	}
	return args;
}

// Generate the benchmark call arguments for layout wrapper style
function genLayoutArgs( params, classification ) {
	var args = [];
	var i, p, lower;

	for ( i = 0; i < params.length; i++ ) {
		p = params[ i ];
		lower = p.toLowerCase();

		if ( lower === 'order' ) {
			args.push( '\'row-major\'' );
		} else if ( STRING_PARAMS[ lower ] ) {
			args.push( STRING_PARAMS[ lower ] );
		} else if ( p === 'select' ) {
			args.push( 'selectFcn' );
		} else if ( lower === 'lwork' ) {
			args.push( '-1' );
		} else if ( p === 'N' || p === 'M' || p === 'K' || p === 'nrhs' || p === 'ncvt' || p === 'nru' || p === 'ncc' || p === 'mm' ) {
			args.push( 'N' );
		} else if ( p === 'kl' || p === 'ku' || p === 'kb' ) {
			args.push( '1' );
		} else if ( p === 'ilo' || p === 'il' || p === 'ifst' || p === 'i1' ) {
			args.push( '1' );
		} else if ( p === 'ihi' || p === 'iu' || p === 'ilst' || p === 'i2' ) {
			args.push( 'N' );
		} else if ( p === 'itype' ) {
			args.push( '1' );
		} else if ( p === 'nsplit' ) {
			args.push( '1' );
		} else if ( p === 'j1' || p === 'n1' || p === 'n2' ) {
			args.push( '1' );
		} else if ( p === 'iter' ) {
			args.push( '0' );
		} else if ( p === 'sdim' || p === 'rank' ) {
			args.push( '0' );
		} else if ( classification.arrays2D.indexOf( p ) >= 0 || classification.arrays1D.indexOf( p ) >= 0 ) {
			args.push( p );
		} else if ( SCALAR_PARAMS.has( lower ) ) {
			args.push( '1.0' );
		} else if ( lower === 'za' || lower === 'z' ) {
			args.push( 'za' );
		} else if ( /^LD[A-Z]+$/.test( p ) || /^ld[A-Z]/.test( p ) ) {
			args.push( 'N' );
		} else if ( /^stride/.test( p ) ) {
			args.push( '1' );
		} else if ( INT_PARAMS.has( p ) ) {
			args.push( 'N' );
		} else {
			// Unclassified — treat as scalar with value 1
			args.push( '1' );
		}
	}
	return args;
}

var LICENSE = [
	'/**',
	'* @license Apache-2.0',
	'*',
	'* Copyright (c) 2025 The Stdlib Authors.',
	'*',
	'* Licensed under the Apache License, Version 2.0 (the "License");',
	'* you may not use this file except in compliance with the License.',
	'* You may obtain a copy of the License at',
	'*',
	'*    http://www.apache.org/licenses/LICENSE-2.0',
	'*',
	'* Unless required by applicable law or agreed to in writing, software',
	'* distributed under the License is distributed on an "AS IS" BASIS,',
	'* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.',
	'* See the License for the specific language governing permissions and',
	'* limitations under the License.',
	'*/'
].join( '\n' );

function generateBenchmark( routine, pkgType, requirePath, params, isNdarray, isComplex, knownArrays ) {
	var classification = classifyParams( params, isComplex );

	// For layout wrappers, some arrays don't have strides, so use knownArrays from ndarray
	if ( knownArrays ) {
		var alreadyClassified = new Set( classification.arrays2D.concat( classification.arrays1D ) );
		var i, p;
		for ( i = 0; i < params.length; i++ ) {
			p = params[ i ];
			if ( alreadyClassified.has( p ) ) {
				continue;
			}
			if ( knownArrays.arrays2D.has( p ) ) {
				classification.arrays2D.push( p );
			} else if ( knownArrays.arrays1D.has( p ) ) {
				classification.arrays1D.push( p );
			} else if ( knownArrays.arrays2D.has( p.toUpperCase() ) || knownArrays.arrays2D.has( p.toLowerCase() ) ) {
				classification.arrays2D.push( p );
			} else if ( knownArrays.arrays1D.has( p.toUpperCase() ) || knownArrays.arrays1D.has( p.toLowerCase() ) ) {
				classification.arrays1D.push( p );
			}
		}
		// Remove from others
		var allKnown = new Set( classification.arrays2D.concat( classification.arrays1D ) );
		classification.others = classification.others.filter( function( o ) {
			return !allKnown.has( o );
		});
	}

	var args;
	if ( isNdarray ) {
		args = genNdarrayArgs( params, classification );
	} else {
		args = genLayoutArgs( params, classification );
	}

	var allArrays = classification.arrays2D.concat( classification.arrays1D );
	var arrayType = isComplex ? 'Complex128Array' : 'Float64Array';
	var randomFn = isComplex ? 'uniform' : 'uniform';
	var dtype = isComplex ? 'complex128' : 'float64';
	var benchLabel = isNdarray ? '%s:ndarray:len=%d' : '%s:len=%d';

	var lines = [];
	lines.push( LICENSE );
	lines.push( '' );
	lines.push( '\'use strict\';' );
	lines.push( '' );
	lines.push( '// MODULES //' );
	lines.push( '' );
	lines.push( 'var bench = require( \'@stdlib/bench\' );' );
	lines.push( 'var uniform = require( \'@stdlib/random/array/uniform\' );' );
	lines.push( 'var isnan = require( \'@stdlib/math/base/assert/is-nan\' );' );
	lines.push( 'var pow = require( \'@stdlib/math/base/special/pow\' );' );
	lines.push( 'var format = require( \'@stdlib/string/format\' );' );
	lines.push( 'var pkg = require( \'./../package.json\' ).name;' );
	lines.push( 'var ' + routine + ' = require( \'' + requirePath + '\' );' );
	lines.push( '' );
	lines.push( '' );
	lines.push( '// VARIABLES //' );
	lines.push( '' );
	lines.push( 'var options = {' );
	lines.push( '\t\'dtype\': \'' + dtype + '\'' );
	lines.push( '};' );
	lines.push( '' );
	lines.push( '' );
	lines.push( '// FUNCTIONS //' );
	lines.push( '' );
	lines.push( '/**' );
	lines.push( '* Creates a benchmark function.' );
	lines.push( '*' );
	lines.push( '* @private' );
	lines.push( '* @param {PositiveInteger} len - array length' );
	lines.push( '* @returns {Function} benchmark function' );
	lines.push( '*/' );
	lines.push( 'function createBenchmark( len ) {' );

	// Use N for dimensions
	lines.push( '\tvar N = len;' );

	// Determine array size - N*N for 2D arrays, N for 1D
	var has2D = classification.arrays2D.length > 0;

	// Create arrays
	for ( var i = 0; i < allArrays.length; i++ ) {
		var arrName = allArrays[ i ];
		var size = ( classification.arrays2D.indexOf( arrName ) >= 0 ) ? 'N * N' : 'N';
		lines.push( '\tvar ' + arrName + ' = uniform( ' + size + ', -10.0, 10.0, options );' );
	}

	// Create complex scalar if needed
	var hasComplexScalar = classification.scalars.some( function( s ) {
		return s.toLowerCase() === 'za' || s.toLowerCase() === 'z';
	});
	if ( hasComplexScalar && isComplex ) {
		lines.push( '\tvar za = new Complex128( 1.0, 0.0 );' );
	}

	if ( classification.selectFn ) {
		lines.push( '\tvar selectFcn = function selectFcn() { return true; };' );
	}

	lines.push( '\treturn benchmark;' );
	lines.push( '' );
	lines.push( '\t/**' );
	lines.push( '\t* Benchmark function.' );
	lines.push( '\t*' );
	lines.push( '\t* @private' );
	lines.push( '\t* @param {Benchmark} b - benchmark instance' );
	lines.push( '\t*/' );
	lines.push( '\tfunction benchmark( b ) {' );
	lines.push( '\t\tvar y;' );
	lines.push( '\t\tvar i;' );
	lines.push( '' );
	lines.push( '\t\tb.tic();' );
	lines.push( '\t\tfor ( i = 0; i < b.iterations; i++ ) {' );

	// Format the call - wrap long lines
	var callStr = routine + '( ' + args.join( ', ' ) + ' )';
	lines.push( '\t\t\ty = ' + callStr + ';' );

	lines.push( '\t\t\tif ( isnan( y ) ) {' );
	lines.push( '\t\t\t\tb.fail( \'should not return NaN\' );' );
	lines.push( '\t\t\t}' );
	lines.push( '\t\t}' );
	lines.push( '\t\tb.toc();' );
	lines.push( '\t\tif ( isnan( y ) ) {' );
	lines.push( '\t\t\tb.fail( \'should not return NaN\' );' );
	lines.push( '\t\t}' );
	lines.push( '\t\tb.pass( \'benchmark finished\' );' );
	lines.push( '\t\tb.end();' );
	lines.push( '\t}' );
	lines.push( '}' );
	lines.push( '' );
	lines.push( '' );
	lines.push( '// MAIN //' );
	lines.push( '' );
	lines.push( '/**' );
	lines.push( '* Main execution sequence.' );
	lines.push( '*' );
	lines.push( '* @private' );
	lines.push( '*/' );
	lines.push( 'function main() {' );
	lines.push( '\tvar len;' );
	lines.push( '\tvar min;' );
	lines.push( '\tvar max;' );
	lines.push( '\tvar f;' );
	lines.push( '\tvar i;' );
	lines.push( '' );
	lines.push( '\tmin = 1; // 10^min' );
	lines.push( '\tmax = 6; // 10^max' );
	lines.push( '' );
	lines.push( '\tfor ( i = min; i <= max; i++ ) {' );
	lines.push( '\t\tlen = pow( 10, i );' );
	lines.push( '\t\tf = createBenchmark( len );' );
	lines.push( '\t\tbench( format( \'' + benchLabel + '\', pkg, len ), f );' );
	lines.push( '\t}' );
	lines.push( '}' );
	lines.push( '' );
	lines.push( 'main();' );
	lines.push( '' );

	return lines.join( '\n' );
}

// Process all modules
var types = [ 'blas', 'lapack' ];
var count = 0;
var skipped = [];

types.forEach( function( pkgType ) {
	var baseDir = path.join( 'lib', pkgType, 'base' );
	if ( !fs.existsSync( baseDir ) ) {
		return;
	}
	var routines = fs.readdirSync( baseDir ).filter( function( d ) {
		return fs.existsSync( path.join( baseDir, d, 'package.json' ) );
	});

	routines.forEach( function( routine ) {
		var moduleDir = path.join( baseDir, routine );
		var libDir = path.join( moduleDir, 'lib' );
		var benchDir = path.join( moduleDir, 'benchmark' );
		var isComplex = COMPLEX_PREFIXES.has( routine[ 0 ] );

		// Extract ndarray/base signature
		var basePath = path.join( libDir, 'base.js' );
		var baseParams = extractSignature( basePath );
		if ( !baseParams ) {
			skipped.push( routine + ' (no base.js signature)' );
			return;
		}

		// Create benchmark directory
		if ( !fs.existsSync( benchDir ) ) {
			fs.mkdirSync( benchDir, { recursive: true } );
		}

		// Generate benchmark.ndarray.js
		var ndarrayRequire;
		if ( fs.existsSync( path.join( libDir, 'ndarray.js' ) ) ) {
			ndarrayRequire = './../lib/ndarray.js';
		} else {
			ndarrayRequire = './../lib/base.js';
		}
		// Build array map from base params (has strides) to share with layout
		var knownArrays = buildArrayMap( baseParams );

		var ndarrayBench = generateBenchmark( routine, pkgType, ndarrayRequire, baseParams, true, isComplex, null );
		fs.writeFileSync( path.join( benchDir, 'benchmark.ndarray.js' ), ndarrayBench );

		// Generate benchmark.js (layout wrapper)
		var layoutPath = path.join( libDir, routine + '.js' );
		if ( fs.existsSync( layoutPath ) ) {
			var layoutParams = extractLayoutSignature( layoutPath );
			if ( layoutParams ) {
				var layoutBench = generateBenchmark( routine, pkgType, './../lib/' + routine + '.js', layoutParams, false, isComplex, knownArrays );
				fs.writeFileSync( path.join( benchDir, 'benchmark.js' ), layoutBench );
			}
		}

		count++;
	});
});

console.log( 'Generated benchmarks for ' + count + ' modules' );
if ( skipped.length > 0 ) {
	console.log( 'Skipped:', skipped.join( ', ' ) );
}
