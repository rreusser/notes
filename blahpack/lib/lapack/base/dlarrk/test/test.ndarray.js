'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var base = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarrk.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'base is a function', function t() {
	assert.equal( typeof base, 'function' );
});

test( 'ndarray is a function', function t() {
	assert.equal( typeof ndarrayFn, 'function' );
});

test( 'base: n0_quick (quick return for N=0)', function t() {
	var tc = findCase( 'n0_quick' );
	var d = new Float64Array( [ 0.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = base( 0, 1, -1.0, 1.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-14, 'w' );
	assertClose( r.werr, tc.werr, 1e-14, 'werr' );
});

test( 'base: n1 (single element diagonal)', function t() {
	var tc = findCase( 'n1' );
	var d = new Float64Array( [ 2.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = base( 1, 1, 0.0, 4.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n2_iw1 (N=2, smallest eigenvalue)', function t() {
	var tc = findCase( 'n2_iw1' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = base( 2, 1, 0.0, 5.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n2_iw2 (N=2, largest eigenvalue)', function t() {
	var tc = findCase( 'n2_iw2' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = base( 2, 2, 0.0, 5.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n5_iw1', function t() {
	var tc = findCase( 'n5_iw1' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = base( 5, 1, -10.0, 10.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n5_iw3', function t() {
	var tc = findCase( 'n5_iw3' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = base( 5, 3, -10.0, 10.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n5_iw5', function t() {
	var tc = findCase( 'n5_iw5' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = base( 5, 5, -10.0, 10.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n4_neg_iw2 (negative eigenvalues)', function t() {
	var tc = findCase( 'n4_neg_iw2' );
	var d = new Float64Array( [ -5.0, -3.0, -7.0, -1.0 ] );
	var e2 = new Float64Array( [ 0.25, 0.25, 0.25 ] );
	var r = base( 4, 2, -10.0, 0.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n3_diag_iw2 (diagonal, second eigenvalue)', function t() {
	var tc = findCase( 'n3_diag_iw2' );
	var d = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var e2 = new Float64Array( [ 0.0, 0.0 ] );
	var r = base( 3, 2, 0.0, 4.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-3, 'werr' );
});

test( 'base: n1_loose (loose tolerance)', function t() {
	var tc = findCase( 'n1_loose' );
	var d = new Float64Array( [ 1.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = base( 1, 1, -1.0, 3.0, d, 1, 0, e2, 1, 0, 1e-16, 1e-2 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-10, 'werr' );
});

test( 'base: non-unit strides', function t() {
	var d = new Float64Array( [ 4.0, 999.0, 3.0, 999.0, 2.0, 999.0, 1.0, 999.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 999.0, 1.0, 999.0, 1.0, 999.0, 1.0 ] );
	var r = base( 5, 1, -10.0, 10.0, d, 2, 0, e2, 2, 0, 1e-300, 1e-12 );
	var tc = findCase( 'n5_iw1' );
	assert.equal( r.info, 0 );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'base: non-unit strides with offset', function t() {
	var d = new Float64Array( [ 999.0, 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 999.0, 1.0, 1.0, 1.0, 1.0 ] );
	var r = base( 5, 3, -10.0, 10.0, d, 1, 1, e2, 1, 1, 1e-300, 1e-12 );
	var tc = findCase( 'n5_iw3' );
	assert.equal( r.info, 0 );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'base: negative N quick return', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = base( -1, 1, 0.0, 1.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, 0 );
	assert.equal( r.w, 0.5 );
	assert.equal( r.werr, 0.0 );
});

test( 'base: pivmin clamping branch (tmp1 near zero)', function t() {
	// Mid will hit D[0]=0 during bisection, triggering the |tmp1|<pivmin clamp.
	var d = new Float64Array( [ 0.0, 1.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = base( 2, 1, -2.0, 2.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	assert.equal( r.info, 0 );
	// Eigenvalues of [[0,1],[1,1]] are (1 +/- sqrt(5))/2.
	assert.ok( Math.abs( r.w - ( ( 1.0 - Math.sqrt( 5.0 ) ) / 2.0 ) ) < 1e-10 );
});

test( 'ndarray wrapper: matches base for default strides', function t() {
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	var e2 = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var r = ndarrayFn( 5, 3, -10.0, 10.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	var tc = findCase( 'n5_iw3' );
	assert.equal( r.info, 0 );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'ndarray wrapper: throws for negative N', function t() {
	var d = new Float64Array( [ 1.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	assert.throws( function throws() {
		ndarrayFn( -1, 1, 0.0, 1.0, d, 1, 0, e2, 1, 0, 1e-300, 1e-12 );
	}, RangeError );
});
