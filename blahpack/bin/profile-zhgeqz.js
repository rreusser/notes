#!/usr/bin/env node
'use strict';

// Profile zhgeqz's inner loops using V8's CPU profiler (inspector API).
// Shows per-function and per-line breakdown to identify exactly which
// rotation loops dominate the 50%+ exclusive time.
//
// Usage:
//   node bin/profile-zhgeqz.js
//   node bin/profile-zhgeqz.js --size 200 --iters 10

var inspector = require( 'node:inspector' );
var path = require( 'path' );

var Complex128Array = require( '@stdlib/array/complex128' );
var zggev = require( '../lib/lapack/base/zggev/lib/base.js' );

// CONFIGURATION //

var N = 200;
var ITERS = 5;
var MODE = 'V'; // 'V' for eigenvectors, 'N' for eigenvalues only
var argv = process.argv.slice( 2 );
var i;

for ( i = 0; i < argv.length; i++ ) {
	if ( argv[ i ] === '--size' && argv[ i + 1 ] ) {
		N = parseInt( argv[ i + 1 ], 10 );
		i += 1;
	} else if ( argv[ i ] === '--iters' && argv[ i + 1 ] ) {
		ITERS = parseInt( argv[ i + 1 ], 10 );
		i += 1;
	} else if ( argv[ i ] === '--eigenvalues-only' ) {
		MODE = 'N';
	}
}

// HELPERS //

function randomComplexMatrix( n ) {
	var view;
	var arr;
	var j;

	arr = new Complex128Array( n * n );
	view = new Float64Array( arr.buffer );
	for ( j = 0; j < view.length; j++ ) {
		view[ j ] = ( 2.0 * Math.random() ) - 1.0;
	}
	return arr;
}

function makeDiagDominant( arr, n ) {
	var view;
	var j;

	view = new Float64Array( arr.buffer );
	for ( j = 0; j < n; j++ ) {
		view[ 2 * ( ( j * n ) + j ) ] += n * 2.0;
	}
	return arr;
}

function runWorkload( count ) {
	var ALPHA;
	var BETA;
	var VL;
	var VR;
	var A;
	var B;
	var k;

	for ( k = 0; k < count; k++ ) {
		A = makeDiagDominant( randomComplexMatrix( N ), N );
		B = makeDiagDominant( randomComplexMatrix( N ), N );
		ALPHA = new Complex128Array( N );
		BETA = new Complex128Array( N );
		VL = new Complex128Array( N * N );
		VR = new Complex128Array( N * N );
		zggev( MODE, MODE, N,
			A, 1, N, 0,
			B, 1, N, 0,
			ALPHA, 1, 0,
			BETA, 1, 0,
			VL, 1, N, 0,
			VR, 1, N, 0
		);
	}
}

// SECTION MAPPING //
// Maps line-number ranges in zhgeqz/lib/base.js to named code sections.

var SECTIONS = [
	{ start: 1061, end: 1086, name: 'QZ sweep: left rotation (H+T cols)' },
	{ start: 1088, end: 1104, name: 'QZ sweep: Q rotation (rows)' },
	{ start: 1106, end: 1126, name: 'QZ sweep: zlartg T(j+1,j+1)' },
	{ start: 1128, end: 1144, name: 'QZ sweep: right rotation (H rows)' },
	{ start: 1146, end: 1157, name: 'QZ sweep: right rotation (T rows)' },
	{ start: 1159, end: 1172, name: 'QZ sweep: Z rotation (rows)' },
	{ start: 1037, end: 1059, name: 'QZ sweep: zlartg H(j,j-1)' },
	{ start: 898, end: 1023, name: 'Wilkinson shift computation' },
	{ start: 326, end: 373, name: 'Main loop control + convergence' },
	{ start: 533, end: 593, name: 'scanAndProcess' },
	{ start: 600, end: 677, name: 'handleBothTestsPass' },
	{ start: 682, end: 790, name: 'chaseZeroToBottom' },
	{ start: 416, end: 468, name: 'handleZeroTdiag' },
	{ start: 474, end: 526, name: 'deflateAndExtract' },
	{ start: 803, end: 893, name: 'doQZStep control + shift sel.' },
	{ start: 242, end: 272, name: 'Initialization (zlaset, norms)' },
	{ start: 274, end: 310, name: 'Set eigenvalues outside active' },
	{ start: 382, end: 410, name: 'setEigenvaluesBelow' }
];

function lineToSection( lineNo ) {
	var j;
	for ( j = 0; j < SECTIONS.length; j++ ) {
		if ( lineNo >= SECTIONS[ j ].start && lineNo <= SECTIONS[ j ].end ) {
			return SECTIONS[ j ].name;
		}
	}
	return 'other (line ' + lineNo + ')';
}

// ANALYSIS //

function analyzeProfile( profile, totalMs ) {
	var zhgeqzFile = 'zhgeqz' + path.sep + 'lib' + path.sep + 'base.js';
	// Also match URL-style paths
	var zhgeqzUrlPat = 'zhgeqz/lib/base.js';
	var totalSamples = profile.samples.length;
	var sampleMs = totalMs / totalSamples;
	var allFuncs;
	var topFuncs;
	var zhgeqzNodes;
	var zhgeqzTotalHits;
	var hasPositionTicks;
	var sectionHits;
	var sectionEntries;
	var allLines;
	var topLines;
	var funcHits;
	var funcEntries;
	var node;
	var url;
	var funcName;
	var shortUrl;
	var match;
	var key;
	var tick;
	var section;
	var pct;
	var ms;
	var tf;
	var i;
	var j;
	var k;
	var n;

	// 1. Per-function breakdown across ALL modules
	allFuncs = {};
	for ( i = 0; i < profile.nodes.length; i++ ) {
		node = profile.nodes[ i ];
		if ( node.hitCount === 0 ) {
			continue;
		}
		url = node.callFrame.url;
		funcName = node.callFrame.functionName || '(anonymous)';

		shortUrl = '';
		match = url.match( /\/lib\/(?:blas|lapack)\/base\/([^/]+)\// );
		if ( match ) {
			shortUrl = match[ 1 ];
		} else if ( url.includes( 'cmplx.js' ) ) {
			shortUrl = 'cmplx';
		} else if ( url === '' ) {
			shortUrl = '(native)';
		} else {
			match = url.match( /([^/]+)$/ );
			shortUrl = match ? match[ 1 ] : url;
		}

		key = shortUrl + ':' + funcName;
		if ( !allFuncs[ key ] ) {
			allFuncs[ key ] = { hits: 0, module: shortUrl, func: funcName };
		}
		allFuncs[ key ].hits += node.hitCount;
	}

	topFuncs = Object.values( allFuncs )
		.sort( function sorter( a, b ) {
			return b.hits - a.hits;
		})
		.slice( 0, 30 );

	console.log( '  TOP FUNCTIONS BY SELF TIME (V8 CPU profiler):' );
	console.log( '  ' + '-'.repeat( 68 ) );
	console.log(
		'  ' + 'Module:Function'.padEnd( 42 ) +
		'Hits'.padStart( 8 ) +
		'Self%'.padStart( 8 ) +
		'Time(ms)'.padStart( 10 )
	);
	console.log( '  ' + '-'.repeat( 68 ) );

	for ( i = 0; i < topFuncs.length; i++ ) {
		tf = topFuncs[ i ];
		pct = ( tf.hits / totalSamples * 100 ).toFixed( 1 );
		ms = ( tf.hits * sampleMs ).toFixed( 1 );
		if ( parseFloat( pct ) < 0.3 ) {
			continue;
		}
		console.log(
			'  ' + ( tf.module + ':' + tf.func ).padEnd( 42 ) +
			String( tf.hits ).padStart( 8 ) +
			( pct + '%' ).padStart( 8 ) +
			ms.padStart( 10 )
		);
	}

	// 2. zhgeqz-specific line-level breakdown
	zhgeqzNodes = [];
	for ( i = 0; i < profile.nodes.length; i++ ) {
		node = profile.nodes[ i ];
		if ( node.hitCount > 0 &&
			( node.callFrame.url.includes( zhgeqzFile ) ||
				node.callFrame.url.includes( zhgeqzUrlPat ) ) ) {
			zhgeqzNodes.push( node );
		}
	}

	zhgeqzTotalHits = 0;
	for ( i = 0; i < zhgeqzNodes.length; i++ ) {
		zhgeqzTotalHits += zhgeqzNodes[ i ].hitCount;
	}

	hasPositionTicks = false;
	for ( i = 0; i < zhgeqzNodes.length; i++ ) {
		if ( zhgeqzNodes[ i ].positionTicks && zhgeqzNodes[ i ].positionTicks.length > 0 ) {
			hasPositionTicks = true;
			break;
		}
	}

	console.log( '' );
	console.log( '' );
	console.log( '  ZHGEQZ LINE-LEVEL BREAKDOWN (' + zhgeqzTotalHits + ' self-time samples, ' +
		( zhgeqzTotalHits * sampleMs ).toFixed( 1 ) + 'ms):' );
	console.log( '  ' + '-'.repeat( 73 ) );

	if ( hasPositionTicks ) {
		// Aggregate by code section
		sectionHits = {};
		for ( i = 0; i < zhgeqzNodes.length; i++ ) {
			n = zhgeqzNodes[ i ];
			if ( !n.positionTicks ) {
				continue;
			}
			for ( j = 0; j < n.positionTicks.length; j++ ) {
				tick = n.positionTicks[ j ];
				section = lineToSection( tick.line );
				sectionHits[ section ] = ( sectionHits[ section ] || 0 ) + tick.ticks;
			}
		}

		sectionEntries = Object.entries( sectionHits )
			.sort( function sorter( a, b ) {
				return b[ 1 ] - a[ 1 ];
			});

		console.log(
			'  ' + 'Code Section'.padEnd( 42 ) +
			'Ticks'.padStart( 8 ) +
			'of zhgeqz'.padStart( 11 ) +
			'of total'.padStart( 10 )
		);
		console.log( '  ' + '-'.repeat( 71 ) );

		for ( i = 0; i < sectionEntries.length; i++ ) {
			var sName = sectionEntries[ i ][ 0 ];
			var sHits = sectionEntries[ i ][ 1 ];
			var sPctZh = ( sHits / zhgeqzTotalHits * 100 ).toFixed( 1 );
			var sPctAll = ( sHits / totalSamples * 100 ).toFixed( 1 );
			if ( parseFloat( sPctAll ) < 0.3 ) {
				continue;
			}
			console.log(
				'  ' + sName.padEnd( 42 ) +
				String( sHits ).padStart( 8 ) +
				( sPctZh + '%' ).padStart( 11 ) +
				( sPctAll + '%' ).padStart( 10 )
			);
		}

		// Top individual lines
		console.log( '' );
		console.log( '  Top 20 individual lines:' );
		allLines = {};
		for ( i = 0; i < zhgeqzNodes.length; i++ ) {
			n = zhgeqzNodes[ i ];
			if ( !n.positionTicks ) {
				continue;
			}
			for ( j = 0; j < n.positionTicks.length; j++ ) {
				tick = n.positionTicks[ j ];
				allLines[ tick.line ] = ( allLines[ tick.line ] || 0 ) + tick.ticks;
			}
		}

		topLines = Object.entries( allLines )
			.sort( function sorter( a, b ) {
				return b[ 1 ] - a[ 1 ];
			})
			.slice( 0, 20 );

		for ( i = 0; i < topLines.length; i++ ) {
			var ln = parseInt( topLines[ i ][ 0 ], 10 );
			var lHits = topLines[ i ][ 1 ];
			var lPctZh = ( lHits / zhgeqzTotalHits * 100 ).toFixed( 1 );
			console.log(
				'    Line ' + String( ln ).padStart( 5 ) + ': ' +
				String( lHits ).padStart( 6 ) + ' ticks (' +
				lPctZh + '%)  [' + lineToSection( ln ) + ']'
			);
		}
	} else {
		// No positionTicks — show function-level only
		console.log( '  (positionTicks not available — function-level breakdown only)' );
		console.log( '' );

		funcHits = {};
		for ( i = 0; i < zhgeqzNodes.length; i++ ) {
			n = zhgeqzNodes[ i ];
			funcName = n.callFrame.functionName || '(zhgeqz top-level)';
			funcHits[ funcName ] = ( funcHits[ funcName ] || 0 ) + n.hitCount;
		}

		funcEntries = Object.entries( funcHits )
			.sort( function sorter( a, b ) {
				return b[ 1 ] - a[ 1 ];
			});

		console.log(
			'  ' + 'Function'.padEnd( 30 ) +
			'Hits'.padStart( 8 ) +
			'of zhgeqz'.padStart( 11 ) +
			'Time(ms)'.padStart( 10 )
		);
		console.log( '  ' + '-'.repeat( 59 ) );

		for ( i = 0; i < funcEntries.length; i++ ) {
			var fName = funcEntries[ i ][ 0 ];
			var fHits = funcEntries[ i ][ 1 ];
			var fPct = ( fHits / zhgeqzTotalHits * 100 ).toFixed( 1 );
			ms = ( fHits * sampleMs ).toFixed( 1 );
			console.log(
				'  ' + fName.padEnd( 30 ) +
				String( fHits ).padStart( 8 ) +
				( fPct + '%' ).padStart( 11 ) +
				ms.padStart( 10 )
			);
		}
	}

	console.log( '' );
}

// MAIN //

function main() {
	var session;
	var totalMs;
	var t0;

	console.log( '' );
	console.log( '='.repeat( 74 ) );
	console.log( '  ZHGEQZ INNER LOOP PROFILING' );
	console.log( '  N=' + N + ', ' + ITERS + ' iterations, mode=' + MODE + '/' + MODE );
	console.log( '='.repeat( 74 ) );

	// Warmup JIT
	console.log( '\n  Warming up JIT (3 iterations)...' );
	runWorkload( 3 );

	// Start CPU profiler via inspector
	session = new inspector.Session();
	session.connect();

	session.post( 'Profiler.enable', function onEnabled( err ) {
		if ( err ) {
			throw err;
		}
		session.post( 'Profiler.setSamplingInterval', { interval: 100 }, function onInterval( err2 ) {
			if ( err2 ) {
				throw err2;
			}
			session.post( 'Profiler.start', function onStarted( err3 ) {
				if ( err3 ) {
					throw err3;
				}
				console.log( '  Running profiled workload (' + ITERS + ' iterations)...' );
				t0 = performance.now();
				runWorkload( ITERS );
				totalMs = performance.now() - t0;

				session.post( 'Profiler.stop', function onStopped( err4, result ) {
					if ( err4 ) {
						throw err4;
					}
					session.post( 'Profiler.disable' );
					session.disconnect();

					console.log( '  Done: ' + totalMs.toFixed( 1 ) + 'ms total (' +
						( totalMs / ITERS ).toFixed( 1 ) + 'ms/call)\n' );

					analyzeProfile( result.profile, totalMs );
				});
			});
		});
	});
}

main();
