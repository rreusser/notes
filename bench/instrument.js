'use strict';

// Generic LAPACK/BLAS subroutine profiler.
//
// Hooks into Module._load to wrap every base.js function with timing
// instrumentation. Tracks inclusive time (wall clock), exclusive time
// (minus children), call counts, and caller→callee edges.
//
// Usage:
//   var instrument = require('./instrument.js');
//   instrument.install();            // BEFORE loading target routines
//   var zggev = require('../lib/lapack/base/zggev/lib/base.js');
//   // ... run workload ...
//   instrument.printReport(totalMs);
//   instrument.printCallTree('zggev');

var Module = require( 'module' );
var path = require( 'path' );

var LIB_ROOT = path.resolve( __dirname, '..', 'lib' );
var origLoad = Module._load;

// Shared mutable state — closures in wrapped functions reference these
// directly, so reset() must mutate in place (not replace).
var stats = {};
var callGraph = {};
var callStack = [];
var installed = false;

// FUNCTIONS //

/**
* Extract routine name from a resolved file path.
*
* Expected pattern:
*   <LIB_ROOT>/blas/base/<routine>/lib/base.js
*   <LIB_ROOT>/lapack/base/<routine>/lib/base.js
*
* @private
* @param {string} filepath - absolute file path
* @returns {string|null} routine name, or null if not a match
*/
function extractName( filepath ) {
	var parts;
	var rel;

	if ( !filepath.startsWith( LIB_ROOT ) ) {
		return null;
	}
	rel = path.relative( LIB_ROOT, filepath );
	parts = rel.split( path.sep );

	// blas/base/<routine>/lib/base.js  →  5 parts
	// lapack/base/<routine>/lib/base.js  →  5 parts
	if (
		parts.length === 5 &&
		parts[ 1 ] === 'base' &&
		parts[ 3 ] === 'lib' &&
		parts[ 4 ] === 'base.js'
	) {
		return parts[ 2 ];
	}
	return null;
}

/**
* Create a timing wrapper around a LAPACK/BLAS function.
*
* @private
* @param {string} name - routine name
* @param {Function} fn - original function
* @returns {Function} wrapped function
*/
function wrapFunction( name, fn ) {
	var entry;

	if ( !stats[ name ] ) {
		stats[ name ] = { calls: 0, inclusive: 0, exclusive: 0 };
	}
	entry = stats[ name ];

	function wrapped() {
		var parentName;
		var elapsed;
		var frame;
		var start;
		var edge;
		var ret;

		entry.calls += 1;
		start = performance.now();
		parentName = ( callStack.length > 0 ) ?
			callStack[ callStack.length - 1 ].name :
			null;
		callStack.push( { name: name, childTime: 0 } );

		ret = fn.apply( this, arguments );

		elapsed = performance.now() - start;
		frame = callStack.pop();
		entry.inclusive += elapsed;
		entry.exclusive += elapsed - frame.childTime;

		// Charge our time to the parent frame
		if ( callStack.length > 0 ) {
			callStack[ callStack.length - 1 ].childTime += elapsed;
		}

		// Record caller→callee edge
		if ( parentName !== null ) {
			edge = parentName + '->' + name;
			if ( !callGraph[ edge ] ) {
				callGraph[ edge ] = { calls: 0, time: 0 };
			}
			callGraph[ edge ].calls += 1;
			callGraph[ edge ].time += elapsed;
		}

		return ret;
	}
	wrapped.__instrumented = true;
	wrapped.__original = fn;
	return wrapped;
}

// MAIN //

/**
* Install the require hook. Must be called BEFORE loading target routines.
*/
function install() {
	if ( installed ) {
		return;
	}
	installed = true;

	Module._load = function hookedLoad( request, parent, isMain ) {
		var resolved;
		var exports;
		var wrapped;
		var name;

		exports = origLoad.call( this, request, parent, isMain );

		if ( typeof exports === 'function' && !exports.__instrumented ) {
			try {
				resolved = Module._resolveFilename( request, parent, isMain );
				name = extractName( resolved );
				if ( name !== null ) {
					wrapped = wrapFunction( name, exports );
					require.cache[ resolved ].exports = wrapped;
					return wrapped;
				}
			} catch ( e ) {
				// resolution failed — skip
			}
		}
		return exports;
	};
}

/**
* Reset all counters between benchmark runs.
* Mutates in place so closures in wrapped functions stay connected.
*/
function reset() {
	var name;
	var edge;
	for ( name in stats ) {
		stats[ name ].calls = 0;
		stats[ name ].inclusive = 0;
		stats[ name ].exclusive = 0;
	}
	for ( edge in callGraph ) {
		delete callGraph[ edge ];
	}
	callStack.length = 0;
}

/**
* Return a snapshot (deep copy) of current stats for later comparison.
*
* @returns {Object} cloned stats
*/
function snapshot() {
	var copy = {};
	var name;
	for ( name in stats ) {
		if ( stats[ name ].calls > 0 ) {
			copy[ name ] = {
				calls: stats[ name ].calls,
				inclusive: stats[ name ].inclusive,
				exclusive: stats[ name ].exclusive
			};
		}
	}
	return copy;
}

/**
* Return the top-K routines by inclusive time from a stats snapshot.
*
* @param {Object} snap - stats snapshot
* @param {number} k - number of routines to return
* @returns {Array} array of { name, calls, inclusive, exclusive }
*/
function topK( snap, k ) {
	return Object.keys( snap )
		.map( function mapper( name ) {
			return {
				name: name,
				calls: snap[ name ].calls,
				inclusive: snap[ name ].inclusive,
				exclusive: snap[ name ].exclusive
			};
		})
		.sort( function sorter( a, b ) {
			return b.inclusive - a.inclusive;
		})
		.slice( 0, k );
}

// REPORTING //

/**
* Print a flat profiling report ranked by inclusive time.
*
* @param {number} totalMs - total wall-clock time for the benchmark
* @param {Object} [opts] - options
* @param {number} [opts.minPercent=0.5] - hide routines below this %
*/
function printReport( totalMs, opts ) {
	var minPercent;
	var entries;
	var inclPct;
	var exclPct;
	var name;
	var s;
	var i;

	opts = opts || {};
	minPercent = opts.minPercent || 0.5;

	entries = Object.keys( stats )
		.map( function mapper( n ) {
			return [ n, stats[ n ] ];
		})
		.filter( function predicate( e ) {
			return e[ 1 ].calls > 0;
		})
		.sort( function sorter( a, b ) {
			return b[ 1 ].inclusive - a[ 1 ].inclusive;
		});

	if ( entries.length === 0 ) {
		console.log( '  (no instrumented calls recorded)' );
		return;
	}

	console.log( '' );
	console.log(
		'  ' +
		'Routine'.padEnd( 14 ) +
		'Calls'.padStart( 7 ) +
		'Incl(ms)'.padStart( 11 ) +
		'Excl(ms)'.padStart( 11 ) +
		'Incl%'.padStart( 8 ) +
		'Excl%'.padStart( 8 ) +
		'us/call'.padStart( 10 )
	);
	console.log( '  ' + '-'.repeat( 69 ) );

	for ( i = 0; i < entries.length; i++ ) {
		name = entries[ i ][ 0 ];
		s = entries[ i ][ 1 ];
		inclPct = ( s.inclusive / totalMs ) * 100;
		exclPct = ( s.exclusive / totalMs ) * 100;

		if ( inclPct < minPercent && exclPct < minPercent ) {
			continue;
		}
		console.log(
			'  ' +
			name.padEnd( 14 ) +
			String( s.calls ).padStart( 7 ) +
			s.inclusive.toFixed( 1 ).padStart( 11 ) +
			s.exclusive.toFixed( 1 ).padStart( 11 ) +
			( inclPct.toFixed( 1 ) + '%' ).padStart( 8 ) +
			( exclPct.toFixed( 1 ) + '%' ).padStart( 8 ) +
			( ( s.inclusive / s.calls ) * 1000 ).toFixed( 0 ).padStart( 10 )
		);
	}
	console.log( '' );
}

/**
* Print a call tree rooted at a given routine.
*
* @param {string} rootName - name of the root routine
* @param {Object} [opts] - options
* @param {number} [opts.maxDepth=3] - maximum tree depth
* @param {number} [opts.minPercent=2.0] - hide children below this % of root
*/
function printCallTree( rootName, opts ) {
	var rootStats;
	var rootTime;
	var maxDepth;
	var minPct;

	opts = opts || {};
	maxDepth = opts.maxDepth || 3;
	minPct = opts.minPercent || 2.0;
	rootStats = stats[ rootName ];

	if ( !rootStats || rootStats.inclusive === 0 ) {
		console.log( '  (no data for ' + rootName + ')' );
		return;
	}

	rootTime = rootStats.inclusive;
	console.log( '  Call tree (' + rootName + ', ' + rootTime.toFixed( 1 ) + 'ms total):' );
	console.log( '' );

	printNode( rootName, '', true, 0, rootTime );
	console.log( '' );

	/**
	* Print a node in the call tree.
	*
	* @private
	* @param {string} name - routine name
	* @param {string} prefix - indentation prefix
	* @param {boolean} isLast - whether this is the last sibling
	* @param {number} depth - current depth
	* @param {number} displayTime - time to display (edge time, or total for root)
	*/
	function printNode( name, prefix, isLast, depth, displayTime ) {
		var childEdgeTime;
		var childPrefix;
		var connector;
		var children;
		var childPct;
		var fraction;
		var parts;
		var edge;
		var pct;
		var s;
		var i;

		s = stats[ name ];
		if ( !s ) {
			return;
		}

		pct = ( ( displayTime / rootTime ) * 100 ).toFixed( 0 );

		if ( depth === 0 ) {
			connector = '  ';
		} else {
			connector = prefix + ( isLast ? '\\-- ' : '|-- ' );
		}

		console.log(
			connector + name +
			'  ' + displayTime.toFixed( 1 ) + 'ms (' + pct + '%)'
		);

		if ( depth >= maxDepth ) {
			return;
		}

		// Scale factor: what fraction of this routine's total time does
		// this call-path represent? Used to proportionally attribute
		// children's time when a routine is called from multiple parents.
		fraction = ( s.inclusive > 0 ) ? ( displayTime / s.inclusive ) : 0;

		// Collect children from call graph, scaling by fraction
		children = [];
		for ( edge in callGraph ) {
			parts = edge.split( '->' );
			if ( parts[ 0 ] === name ) {
				childEdgeTime = callGraph[ edge ].time * fraction;
				childPct = ( childEdgeTime / rootTime ) * 100;
				if ( childPct >= minPct ) {
					children.push( {
						name: parts[ 1 ],
						time: childEdgeTime,
						calls: callGraph[ edge ].calls
					});
				}
			}
		}
		children.sort( function sorter( a, b ) {
			return b.time - a.time;
		});

		childPrefix = ( depth === 0 ) ?
			'  ' :
			prefix + ( isLast ? '    ' : '|   ' );

		for ( i = 0; i < children.length; i++ ) {
			printNode(
				children[ i ].name,
				childPrefix,
				i === children.length - 1,
				depth + 1,
				children[ i ].time
			);
		}
	}
}

// EXPORTS //

module.exports = {
	install: install,
	reset: reset,
	snapshot: snapshot,
	topK: topK,
	getStats: function getStats() {
		return stats;
	},
	getCallGraph: function getCallGraph() {
		return callGraph;
	},
	printReport: printReport,
	printCallTree: printCallTree
};
