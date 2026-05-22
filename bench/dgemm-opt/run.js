'use strict';

/* eslint-disable max-len */

// Benchmark runner. Sweeps a set of matrix shapes for a chosen transpose/layout
// configuration, times every variant with the min-of-trials harness, and emits
// a markdown table (GFLOPS + speedup vs reference).

var fs = require( 'fs' );
var path = require( 'path' );
var harness = require( './harness.js' );

function randF64( n ) {
	var a = new Float64Array( n );
	var i;
	for ( i = 0; i < n; i++ ) {
		a[ i ] = ( Math.random() * 20 ) - 10;
	}
	return a;
}

// Column-major contiguous layout: A is rows x cols, leading dim = rows.
function strides( rows, cols, layout ) {
	if ( layout === 'row' ) {
		return { 's1': cols, 's2': 1, 'n': rows*cols };
	}
	return { 's1': 1, 's2': rows, 'n': rows*cols };
}

// Build a benchmark closure for one variant on one (M,N,K, trans, layout) case.
function makeFn( fn, cfg ) {
	var nota = ( cfg.transa === 'no-transpose' );
	var notb = ( cfg.transb === 'no-transpose' );
	var aRows = nota ? cfg.M : cfg.K;
	var aCols = nota ? cfg.K : cfg.M;
	var bRows = notb ? cfg.K : cfg.N;
	var bCols = notb ? cfg.N : cfg.K;
	var sa = strides( aRows, aCols, cfg.layout );
	var sb = strides( bRows, bCols, cfg.layout );
	var sc = strides( cfg.M, cfg.N, cfg.layout );
	var A = randF64( sa.n );
	var B = randF64( sb.n );
	var C = randF64( sc.n );
	var alpha = cfg.alpha;
	var beta = cfg.beta;
	var M = cfg.M;
	var N = cfg.N;
	var K = cfg.K;
	var ta = cfg.transa;
	var tb = cfg.transb;
	return function () {
		fn( ta, tb, M, N, K, alpha, A, sa.s1, sa.s2, 0, B, sb.s1, sb.s2, 0, beta, C, sc.s1, sc.s2, 0 );
	};
}

function gflops( M, N, K, ns ) {
	return ( 2 * M * N * K ) / ns; // 2*MNK flops / ns  == Gflop/s
}

function loadVariants( names ) {
	return names.map( function ( name ) {
		return { 'name': name, 'fn': require( './variants/' + name + '.js' ) };
	} );
}

// shapes: array of {M,N,K,label}
function sweep( opts ) {
	var variants = loadVariants( opts.variants );
	var rows = [];
	opts.shapes.forEach( function ( sh ) {
		var cfg = {
			'M': sh.M, 'N': sh.N, 'K': sh.K,
			'transa': opts.transa || 'no-transpose',
			'transb': opts.transb || 'no-transpose',
			'layout': opts.layout || 'col',
			'alpha': ( opts.alpha === undefined ) ? 1.0 : opts.alpha,
			'beta': ( opts.beta === undefined ) ? 1.0 : opts.beta
		};
		var bound = variants.map( function ( v ) {
			return { 'name': v.name, 'fn': makeFn( v.fn, cfg ) };
		} );
		var res = harness.benchmark({
			'variants': bound,
			'trials': opts.trials || 15,
			'targetMs': opts.targetMs || 50,
			'warmup': 3
		});
		var row = { 'label': sh.label || ( sh.M+'x'+sh.N+'x'+sh.K ), 'M': sh.M, 'N': sh.N, 'K': sh.K, 'variants': {} };
		opts.variants.forEach( function ( name ) {
			row.variants[ name ] = {
				'minNs': res[name].minNs,
				'gflops': gflops( sh.M, sh.N, sh.K, res[name].minNs )
			};
		} );
		rows.push( row );
		// progress to stderr:
		process.stderr.write( '  done ' + row.label + '\n' );
	} );
	return rows;
}

module.exports = { sweep, makeFn, gflops, loadVariants };
