'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgecon = require( './../lib/base.js' );
var zgetrf = require( '../../zgetrf/lib/base.js' );
var zlange = require( '../../zlange/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgecon.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

// Helper to compute condition number
function computeRcond( normStr, N, Aflat ) {
	var A = new Complex128Array( Aflat );
	var work = new Complex128Array( 2 * N );
	var rwork = new Float64Array( 2 * N );
	var ipiv = new Int32Array( N );
	var rcond = new Float64Array( 1 );
	var anorm = zlange( normStr, N, N, A, 1, N, 0, rwork, 1, 0 );
	zgetrf( N, N, A, 1, N, 0, ipiv, 1, 0 );
	var info = zgecon( normStr, N, A, 1, N, 0, anorm, rcond, work, 1, 0, rwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}


// TESTS //

test( 'zgecon: main export is a function', function t() {
	assert.strictEqual( typeof zgecon, 'function' );
});

test( 'zgecon: well-conditioned 3x3 hermitian (1-norm)', function t() {
	var tc = findCase( 'well_cond_1norm' );
	// A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]
	var result = computeRcond( 'one-norm', 3, [
		4, 0,   1, -1,  0, 0,
		1, 1,   3, 0,   1, 0,
		0, 0,   1, 0,   2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgecon: well-conditioned 3x3 (infinity-norm)', function t() {
	var tc = findCase( 'well_cond_Inorm' );
	var result = computeRcond( 'inf-norm', 3, [
		4, 0,   1, -1,  0, 0,
		1, 1,   3, 0,   1, 0,
		0, 0,   1, 0,   2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgecon: 3x3 identity (rcond=1)', function t() {
	var tc = findCase( 'identity' );
	var result = computeRcond( 'one-norm', 3, [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgecon: 3x3 ill-conditioned', function t() {
	var tc = findCase( 'ill_cond' );
	var result = computeRcond( 'one-norm', 3, [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1e-15, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgecon: N=0 (rcond=1)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var work = new Complex128Array( 1 );
	var rwork = new Float64Array( 1 );
	var rcond = new Float64Array( 1 );
	var info = zgecon( 'one-norm', 0, A, 1, 1, 0, 0.0, rcond, work, 1, 0, rwork, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'zgecon: anorm=0 (rcond=0)', function t() {
	var tc = findCase( 'anorm_zero' );
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	var work = new Complex128Array( 6 );
	var rwork = new Float64Array( 6 );
	var ipiv = new Int32Array( 3 );
	var rcond = new Float64Array( 1 );
	zgetrf( 3, 3, A, 1, 3, 0, ipiv, 1, 0 );
	var info = zgecon( 'one-norm', 3, A, 1, 3, 0, 0.0, rcond, work, 1, 0, rwork, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'zgecon: 4x4 complex matrix (1-norm)', function t() {
	var tc = findCase( '4x4_1norm' );
	var result = computeRcond( 'one-norm', 4, [
		5, 1,   1, 0,   0, 0,   0, 0,
		1, 0,   4, -1,  1, 1,   0, 0,
		0, 0,   1, -1,  3, 0,   1, 0,
		0, 0,   0, 0,   1, 0,   2, 1
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zgecon: 4x4 complex matrix (inf-norm)', function t() {
	var tc = findCase( '4x4_Inorm' );
	var result = computeRcond( 'inf-norm', 4, [
		5, 1,   1, 0,   0, 0,   0, 0,
		1, 0,   4, -1,  1, 1,   0, 0,
		0, 0,   1, -1,  3, 0,   1, 0,
		0, 0,   0, 0,   1, 0,   2, 1
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});
