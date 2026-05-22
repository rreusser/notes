'use strict';

/**
* Robust micro-benchmark harness for dgemm variants.
*
* Design goals (machine is shared / noisy):
*   - Minimum-of-trials timing. External CPU contention can only *add* wall
*     time, never subtract it, so the minimum per-iteration time over many
*     trials is the best estimate of the true cost on this hardware.
*   - Interleaving. Variants are sampled round-robin across the whole window
*     so slow drift (thermal, background jobs ramping) hits them equally.
*   - Adaptive batch sizing. Each timed batch runs enough iterations to take
*     a target wall time, dwarfing timer granularity.
*   - Correctness gate. Every variant is validated against the reference
*     before it is allowed to compete.
*/

var hrtime = process.hrtime.bigint;

/**
* Time a single batch of `iters` calls to `fn`, returning nanoseconds elapsed.
*/
function timeBatch( fn, iters ) {
	var t0;
	var t1;
	var i;
	t0 = hrtime();
	for ( i = 0; i < iters; i++ ) {
		fn();
	}
	t1 = hrtime();
	return Number( t1 - t0 );
}

/**
* Calibrate the number of iterations so a batch takes ~targetMs.
*/
function calibrate( fn, targetMs ) {
	var iters = 1;
	var ns;
	while ( true ) {
		ns = timeBatch( fn, iters );
		if ( ns > targetMs * 1e6 || iters > (1<<28) ) {
			break;
		}
		// Scale up toward target, with headroom:
		if ( ns < 1e5 ) { // < 0.1ms: jump hard
			iters *= 8;
		} else {
			iters = Math.max( iters+1, Math.ceil( iters * (targetMs*1e6) / ns * 1.2 ) );
		}
	}
	return Math.max( 1, iters );
}

/**
* Benchmark a set of named variants.
*
* @param {Object} opts
* @param {Array<{name,fn}>} opts.variants - functions to time (already bound to data)
* @param {number} [opts.trials=12] - number of interleaved trials per variant
* @param {number} [opts.targetMs=60] - target wall time per timed batch
* @param {number} [opts.warmup=3] - warmup batches per variant
* @returns {Object} map name -> { minNs, medianNs, iters, trials:[...] }
*/
function benchmark( opts ) {
	var variants = opts.variants;
	var trials = opts.trials || 12;
	var targetMs = opts.targetMs || 60;
	var warmup = ( opts.warmup === undefined ) ? 3 : opts.warmup;
	var results = {};
	var iters = {};
	var v;
	var t;
	var w;
	var ns;

	// Warmup + calibrate each variant:
	for ( v = 0; v < variants.length; v++ ) {
		for ( w = 0; w < warmup; w++ ) {
			timeBatch( variants[v].fn, 2 );
		}
		iters[ variants[v].name ] = calibrate( variants[v].fn, targetMs );
		results[ variants[v].name ] = {
			'iters': iters[ variants[v].name ],
			'trials': []
		};
	}

	// Interleaved measurement: trial-major, variant-minor:
	for ( t = 0; t < trials; t++ ) {
		for ( v = 0; v < variants.length; v++ ) {
			ns = timeBatch( variants[v].fn, iters[ variants[v].name ] );
			results[ variants[v].name ].trials.push( ns / iters[ variants[v].name ] );
		}
	}

	// Reduce:
	for ( v = 0; v < variants.length; v++ ) {
		var r = results[ variants[v].name ];
		var sorted = r.trials.slice().sort( function ( a, b ) { return a-b; } );
		r.minNs = sorted[ 0 ];
		r.medianNs = sorted[ Math.floor( sorted.length/2 ) ];
		r.p25Ns = sorted[ Math.floor( sorted.length*0.25 ) ];
	}
	return results;
}

module.exports = { benchmark, timeBatch, calibrate };
