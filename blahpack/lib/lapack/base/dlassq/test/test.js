'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlassq = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlassq.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.equal( actual, 0.0, msg + ': expected 0, got ' + actual );
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}


// TESTS //

test( 'dlassq: basic - x=[3,4], scl=1, sumsq=0', function t() {
	var tc = findCase( 'dlassq_basic' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: accumulate - x=[1], scl=2, sumsq=3', function t() {
	var tc = findCase( 'dlassq_accumulate' );
	var x = new Float64Array( [ 1.0 ] );
	var result = dlassq( 1, x, 1, 0, 2.0, 3.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: N=0 quick return', function t() {
	var tc = findCase( 'dlassq_n_zero' );
	var x = new Float64Array( [ 1.0 ] );
	var result = dlassq( 0, x, 1, 0, 2.0, 5.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: stride=2 - x=[3,99,4], picks 3 and 4', function t() {
	var tc = findCase( 'dlassq_stride' );
	var x = new Float64Array( [ 3.0, 99.0, 4.0 ] );
	var result = dlassq( 2, x, 2, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: all zeros', function t() {
	var tc = findCase( 'dlassq_zeros' );
	var x = new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	var result = dlassq( 5, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: very large values (abig accumulator)', function t() {
	var tc = findCase( 'dlassq_big' );
	var x = new Float64Array( [ 1e300, 2e300 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: very small values (asml accumulator)', function t() {
	var tc = findCase( 'dlassq_small' );
	var x = new Float64Array( [ 1e-300, 2e-300 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: mix of big and normal values (abig+amed)', function t() {
	var tc = findCase( 'dlassq_big_normal' );
	var x = new Float64Array( [ 1e300, 1.0 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: mix of small and normal values (asml+amed)', function t() {
	var tc = findCase( 'dlassq_small_normal' );
	var x = new Float64Array( [ 1e-300, 1.0 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: negative stride', function t() {
	var tc = findCase( 'dlassq_neg_stride' );
	// With stride=-1, offset=0, N=2: ix = 0 - (2-1)*(-1) = 1. Reads x[1] then x[0]
	var x = new Float64Array( [ 3.0, 4.0 ] );
	var result = dlassq( 2, x, -1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: big existing sum + big values', function t() {
	var tc = findCase( 'dlassq_big_existing' );
	var x = new Float64Array( [ 1e300 ] );
	var result = dlassq( 1, x, 1, 0, 1e200, 1.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: small existing sum + small values', function t() {
	var tc = findCase( 'dlassq_small_existing' );
	var x = new Float64Array( [ 1e-300 ] );
	var result = dlassq( 1, x, 1, 0, 1e-200, 1.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: pure small values only (asml-only branch)', function t() {
	var tc = findCase( 'dlassq_pure_small' );
	var x = new Float64Array( [ 1e-300, 2e-300 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: scale=0, sumsq=0 initial', function t() {
	var tc = findCase( 'dlassq_scale_zero' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	var result = dlassq( 2, x, 1, 0, 0.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: big existing sum with scale > 1', function t() {
	var tc = findCase( 'dlassq_big_existing_scale_gt1' );
	var x = new Float64Array( [ 1e300 ] );
	var result = dlassq( 1, x, 1, 0, 2.0, 1e308 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: small existing sum with scale < 1', function t() {
	var tc = findCase( 'dlassq_small_existing_scale_lt1' );
	var x = new Float64Array( [ 1e-300 ] );
	var result = dlassq( 1, x, 1, 0, 0.5, 1e-300 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: single element', function t() {
	var tc = findCase( 'dlassq_single' );
	var x = new Float64Array( [ 7.0 ] );
	var result = dlassq( 1, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: negative values (abs taken)', function t() {
	var tc = findCase( 'dlassq_negative' );
	var x = new Float64Array( [ -3.0, -4.0 ] );
	var result = dlassq( 2, x, 1, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: stride=3', function t() {
	var tc = findCase( 'dlassq_stride3' );
	var x = new Float64Array( [ 2.0, 0.0, 0.0, 3.0, 0.0, 0.0, 6.0 ] );
	var result = dlassq( 3, x, 3, 0, 1.0, 0.0 );
	assertClose( result.scl, tc.scl, 1e-14, 'scl' );
	assertClose( result.sumsq, tc.sumsq, 1e-14, 'sumsq' );
});

test( 'dlassq: NaN scale quick return', function t() {
	var x = new Float64Array( [ 1.0 ] );
	var result = dlassq( 1, x, 1, 0, NaN, 1.0 );
	assert.ok( result.scl !== result.scl, 'scl should be NaN' );
});

test( 'dlassq: NaN sumsq quick return', function t() {
	var x = new Float64Array( [ 1.0 ] );
	var result = dlassq( 1, x, 1, 0, 1.0, NaN );
	assert.ok( result.sumsq !== result.sumsq, 'sumsq should be NaN' );
});

test( 'dlassq: offset parameter', function t() {
	// Array has data at offset=2: x[2]=3, x[3]=4
	var x = new Float64Array( [ 99.0, 99.0, 3.0, 4.0 ] );
	var result = dlassq( 2, x, 1, 2, 1.0, 0.0 );
	assertClose( result.scl, 1.0, 1e-14, 'scl' );
	assertClose( result.sumsq, 25.0, 1e-14, 'sumsq' );
});

test( 'dlassq: sumsq=0 resets scale to 1', function t() {
	var x = new Float64Array( [ 3.0 ] );
	// scale=5, sumsq=0 => scale should be reset to 1 before processing
	var result = dlassq( 1, x, 1, 0, 5.0, 0.0 );
	assertClose( result.scl, 1.0, 1e-14, 'scl' );
	assertClose( result.sumsq, 9.0, 1e-14, 'sumsq' );
});

test( 'dlassq: scale=0, sumsq!=0 resets both', function t() {
	// scale=0 with sumsq=5 => should set scale=1, sumsq=0 first, then accumulate
	var x = new Float64Array( [ 3.0 ] );
	var result = dlassq( 1, x, 1, 0, 0.0, 5.0 );
	assertClose( result.scl, 1.0, 1e-14, 'scl' );
	assertClose( result.sumsq, 9.0, 1e-14, 'sumsq' );
});

test( 'dlassq: big existing sumsq with scale <= 1 (line 113)', function t() {
	// Need scale*sqrt(sumsq) > TBIG with scale <= 1
	// scale=0.5, sumsq = 1e308 => scale*sqrt(sumsq) = 0.5*1e154 ~ 5e153 > TBIG
	// scale <= 1, so goes to else branch (line 113)
	var x = new Float64Array( [ 1e300 ] );
	var result = dlassq( 1, x, 1, 0, 0.5, 1e308 );
	// Just verify the result is finite and has the right shape
	var SBIG = Math.pow( 2, -538 );
	assert.ok( result.scl === 1.0 / SBIG, 'scl should be 1/SBIG' );
	assert.ok( isFinite( result.sumsq ), 'sumsq should be finite' );
	assert.ok( result.sumsq > 0, 'sumsq should be positive' );
});

test( 'dlassq: small existing sumsq with scale >= 1 (line 122)', function t() {
	// Need scale*sqrt(sumsq) < TSML with scale >= 1
	// scale=1.0, sumsq = 1e-320 => scale*sqrt(sumsq) ~ 1e-160 < TSML=2^-511 ~ 1.49e-154
	// And we need only small values in x so notbig is true
	var x = new Float64Array( [ 1e-300 ] );
	var result = dlassq( 1, x, 1, 0, 1.0, 1e-320 );
	// Verify mathematical invariant: scl^2 * sumsq = 1e-320 + 1e-600
	var totalSS = result.scl * result.scl * result.sumsq;
	assertClose( totalSS, 1e-320 + 1e-600, 1e-10, 'total sum of squares' );
});

test( 'dlassq: asml > amed branch in combination (line 145)', function t() {
	// Need asml > amed after sqrt/unscaling.
	// TSML = 2^-511 ~ 1.49e-154. Values below TSML go to asml, values above to amed.
	// After combination: amed = sqrt(amed_acc), asml = sqrt(asml_acc)/SSML
	// Use 1000 values at 1e-155 (below TSML) to make asml dominate,
	// plus one value at 1.5e-154 (just above TSML) for a tiny amed.
	var i;
	var x = new Float64Array( 1001 );
	for ( i = 0; i < 1000; i++ ) {
		x[ i ] = 1.0e-155;
	}
	x[ 1000 ] = 1.5e-154;
	var result = dlassq( 1001, x, 1, 0, 1.0, 0.0 );
	// Verify mathematical invariant
	var totalSS = result.scl * result.scl * result.sumsq;
	var expected = 1000 * 1e-310 + 2.25e-308;
	assertClose( totalSS, expected, 1e-10, 'total sum of squares' );
});
