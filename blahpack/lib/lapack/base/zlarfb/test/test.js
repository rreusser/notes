'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlarfb = require( './../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarfb.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1e-30 );
	assert.ok( relErr <= 1e-10, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, label ) {
	var i;
	assert.strictEqual( actual.length, expected.length, label + ' length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], label + '[' + i + ']' );
	}
}

// M=4, N=3, K=2
// V is 4x2 (LDV=4, unit lower triangular)
function makeV() {
	return new Float64Array( [
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.4, -0.3,
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5
	]);
}

// T is 2x2 upper triangular, LDT=3 (so 3 complex entries per column)
function makeT() {
	var T = new Float64Array( 12 );
	// T(0,0) = 1.2 - 0.3i
	T[ 0 ] = 1.2; T[ 1 ] = -0.3;
	// T(1,0) = 0
	// T(2,0) = 0 (padding)
	// T(0,1) = 0.52 - 0.15i
	T[ 6 ] = 0.52; T[ 7 ] = -0.15;
	// T(1,1) = 1.5 + 0.4i
	T[ 8 ] = 1.5; T[ 9 ] = 0.4;
	return T;
}

// C is 4x3 (LDC=4), col-major interleaved
function makeC() {
	return new Float64Array( [
		1.0, 0.0,   0.0, 1.0,   2.0, -1.0,  3.0, 0.5,
		-1.0, 2.0,  0.5, 0.5,   1.5, -0.5, -2.0, 1.0,
		0.0, 0.0,   1.0, 1.0,  -0.5, 0.0,   2.0, -2.0
	]);
}

test( 'zlarfb: left, no-transpose, forward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_notrans_fwd_col'; });
	var V = makeV();
	var T = makeT();
	var C = makeC();
	var work = new Float64Array( 60 );

	// strideV1=1, strideV2=4, strideT1=1, strideT2=3, strideC1=1, strideC2=4
	// strideW1=1, strideW2=3 (LDWORK=3 for N=3)
	zlarfb( 'L', 'N', 'F', 'C', 4, 3, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, conjugate-transpose, forward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_conjtrans_fwd_col'; });
	var V = makeV();
	var T = makeT();
	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'C', 'F', 'C', 4, 3, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, no-transpose, forward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_notrans_fwd_col'; });
	var V = makeV();
	var T = makeT();
	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	// strideC1=1, strideC2=3 (LDC=3)
	// strideW1=1, strideW2=3 (LDWORK=3 for M=3)
	zlarfb( 'R', 'N', 'F', 'C', 3, 4, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: M=0 (quick return)', function t() {
	var C = new Float64Array( [ 1.0, 0.0 ] );
	var V = makeV();
	var T = makeT();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'N', 'F', 'C', 0, 3, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 1, 0,
		work, 1, 3, 0 );
	assert.strictEqual( C[ 0 ], 1.0 );
	assert.strictEqual( C[ 1 ], 0.0 );
});

test( 'zlarfb: left, no-transpose, backward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_notrans_bwd_col'; });
	// V for backward: last K rows have unit upper triangular
	var V = new Float64Array( [
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	// T for backward is lower triangular
	var T = new Float64Array( 12 );
	T[ 0 ] = 1.2; T[ 1 ] = -0.3;
	T[ 2 ] = -1.22; T[ 3 ] = -1.50;
	// T(2,0) = 0 (padding)
	// T(0,1) = 0
	T[ 8 ] = 1.5; T[ 9 ] = 0.4;
	// T(2,1) = 0 (padding)

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'N', 'B', 'C', 4, 3, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, no-transpose, backward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_notrans_bwd_col'; });
	// V for backward: last K rows have unit upper triangular
	var V = new Float64Array( [
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	// T for backward is lower triangular
	var T = new Float64Array( 12 );
	T[ 0 ] = 1.2; T[ 1 ] = -0.3;
	T[ 2 ] = -1.22; T[ 3 ] = -1.50;
	T[ 8 ] = 1.5; T[ 9 ] = 0.4;

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'N', 'B', 'C', 3, 4, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, conjugate-transpose, backward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_conjtrans_bwd_col'; });
	var V = new Float64Array( [
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	var T = new Float64Array( 12 );
	T[ 0 ] = 1.2; T[ 1 ] = -0.3;
	T[ 2 ] = -1.22; T[ 3 ] = -1.50;
	T[ 8 ] = 1.5; T[ 9 ] = 0.4;

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'C', 'B', 'C', 4, 3, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, conjugate-transpose, forward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_conjtrans_fwd_col'; });
	var V = makeV();
	var T = makeT();
	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'C', 'F', 'C', 3, 4, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, no-transpose, forward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_notrans_fwd_row'; });
	// V is 2x4 (K=2, M=4), unit upper triangular in V1
	var V = new Float64Array( [
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0,
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5
	]);
	// T computed by zlarft('F','R',4,2,V,2,tau,T,3)
	var tcT = fixture.find( function f( t ) { return t.name === 'zlarfb_left_notrans_fwd_row'; });
	// Use pre-computed T from the fixture (we need zlarft output)
	// Actually, let's compute T ourselves using zlarft
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'F', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'N', 'F', 'R', 4, 3, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, no-transpose, forward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_notrans_fwd_row'; });
	// V is 2x4 (K=2, N=4), unit upper triangular in V1
	var V = new Float64Array( [
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0,
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'F', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'N', 'F', 'R', 3, 4, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, conjugate-transpose, forward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_conjtrans_fwd_row'; });
	// V is 2x4 (K=2, M=4), unit upper triangular in V1
	var V = new Float64Array( [
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0,
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'F', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'C', 'F', 'R', 4, 3, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, no-transpose, backward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_notrans_bwd_row'; });
	// V is 2x4 (K=2, M=4), last K cols unit lower triangular for backward
	var V = new Float64Array( [
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5,
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'B', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'N', 'B', 'R', 4, 3, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: left, conjugate-transpose, backward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_left_conjtrans_bwd_row'; });
	// V is 2x4 (K=2, M=4), last K cols unit lower triangular for backward
	var V = new Float64Array( [
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5,
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'B', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	var C = makeC();
	var work = new Float64Array( 60 );

	zlarfb( 'L', 'C', 'B', 'R', 4, 3, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 4, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, no-transpose, backward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_notrans_bwd_row'; });
	// V is 2x4 (K=2, N=4), last K cols unit lower triangular for backward
	var V = new Float64Array( [
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5,
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'B', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'N', 'B', 'R', 3, 4, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, conjugate-transpose, backward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_conjtrans_bwd_row'; });
	// V is 2x4 (K=2, N=4), last K cols unit lower triangular for backward
	var V = new Float64Array( [
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5,
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'B', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'C', 'B', 'R', 3, 4, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, conjugate-transpose, forward, rowwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_conjtrans_fwd_row'; });
	// V is 2x4 (K=2, N=4), unit upper triangular in V1
	var V = new Float64Array( [
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0,
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5
	]);
	var zlarft = require( '../../zlarft/lib/base.js' );
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );
	zlarft( 'F', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'C', 'F', 'R', 3, 4, 2,
		V, 1, 2, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarfb: right, conjugate-transpose, backward, columnwise', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarfb_right_conjtrans_bwd_col'; });
	// V is 4x2 (N=4, K=2), last K rows unit upper triangular for backward
	var V = new Float64Array( [
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	// T for backward is lower triangular
	var T = new Float64Array( 12 );
	T[ 0 ] = 1.2; T[ 1 ] = -0.3;
	T[ 2 ] = -1.22; T[ 3 ] = -1.50;
	T[ 8 ] = 1.5; T[ 9 ] = 0.4;

	// C is 3x4 (M=3, N=4, LDC=3)
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,  3.0, -1.0,
		0.0, 1.0,  0.5, 0.5,  -1.0, 2.0,
		1.5, -0.5, -2.0, 1.0,  0.0, 0.0,
		1.0, 1.0,  -0.5, 0.0,  2.0, -2.0
	]);
	var work = new Float64Array( 60 );

	zlarfb( 'R', 'C', 'B', 'C', 3, 4, 2,
		V, 1, 4, 0,
		T, 1, 3, 0,
		C, 1, 3, 0,
		work, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});
