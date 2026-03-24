'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlatrs = require( './../lib/base.js' );

// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlatrs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


test( 'dlatrs: main export is a function', function t() {
	assert.strictEqual( typeof dlatrs, 'function' );
});

test( 'dlatrs: upper, no-transpose, non-unit diagonal, 3x3', function t() {
	var tc = findCase( 'upper_N_nonunit' );
	// A = [[2, 1, 1], [0, 3, 2], [0, 0, 4]] column-major
	var A = new Float64Array( [ 2, 0, 0, 1, 3, 0, 1, 2, 4 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatrs: lower, no-transpose, non-unit diagonal, 3x3', function t() {
	var tc = findCase( 'lower_N_nonunit' );
	// A = [[2, 0, 0], [1, 3, 0], [1, 2, 4]] column-major
	var A = new Float64Array( [ 2, 1, 1, 0, 3, 2, 0, 0, 4 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatrs: upper, transpose, non-unit diagonal, 3x3', function t() {
	var tc = findCase( 'upper_T_nonunit' );
	var A = new Float64Array( [ 2, 0, 0, 1, 3, 0, 1, 2, 4 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: lower, transpose, non-unit diagonal, 3x3', function t() {
	var tc = findCase( 'lower_T_nonunit' );
	var A = new Float64Array( [ 2, 1, 1, 0, 3, 2, 0, 0, 4 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: upper, no-transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'upper_N_unit' );
	var A = new Float64Array( [ 99, 0, 0, 1, 99, 0, 1, 2, 99 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: N=0', function t() {
	var A = new Float64Array( 0 );
	var x = new Float64Array( 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 0 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 0, A, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 1.0 );
});

test( 'dlatrs: N=1', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( [ 5 ] );
	var x = new Float64Array( [ 10 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 1, A, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( x[ 0 ], tc.x[ 0 ], 1e-14, 'x[0]' );
});

test( 'dlatrs: upper, normin=Y (pre-computed norms), 4x4', function t() {
	var tc = findCase( 'upper_N_normin_Y' );
	var A = new Float64Array( [ 3, 0, 0, 0, 1, 4, 0, 0, 2, 1, 2, 0, 1, 2, 1, 5 ] );
	var x = new Float64Array( [ 1, 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( [ 0, 1, 3, 4 ] );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'yes', 4, A, 1, 4, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: lower, transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'lower_T_unit' );
	var A = new Float64Array( [ 99, 1, 2, 0, 99, 3, 0, 0, 99 ] );
	var x = new Float64Array( [ 6, 5, 4 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: identity matrix', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var x = new Float64Array( [ 7, 8, 9 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatrs: verifies A * x = scale * b', function t() {
	var A = new Float64Array( [ 2, 0, 0, 1, 3, 0, 1, 2, 4 ] );
	var b = new Float64Array( [ 1, 2, 3 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var i;
	var j;
	var ax;
	dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	for ( i = 0; i < 3; i++ ) {
		ax = 0;
		for ( j = 0; j < 3; j++ ) {
			ax += A[ i + j * 3 ] * x[ j ];
		}
		assertClose( ax, scale[ 0 ] * b[ i ], 1e-14, 'A*x[' + i + '] == s*b[' + i + ']' );
	}
});

test( 'dlatrs: lower, no-transpose, unit diagonal', function t() {
	var A = new Float64Array( [ 99, 1, 2, 0, 99, 3, 0, 0, 99 ] );
	var x = new Float64Array( [ 6, 5, 4 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 1.0 );
	// x[0] = 6, x[1] = 5 - 1*6 = -1, x[2] = 4 - 2*6 - 3*(-1) = -5
	assertClose( x[ 0 ], 6, 1e-14, 'x[0]' );
	assertClose( x[ 1 ], -1, 1e-14, 'x[1]' );
	assertClose( x[ 2 ], -5, 1e-14, 'x[2]' );
});

test( 'dlatrs: near-singular upper triangular triggers careful solve (non-transpose)', function t() {
	// Tiny diagonal entries cause grow to drop below SMLNUM, triggering the
	// careful solve path. Use a very small diagonal to force the slow path.
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 1, tiny, 0, 1, 1, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	// With near-singular matrix, scale may be adjusted
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: near-singular lower triangular triggers careful solve (non-transpose)', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 1, 1, 0, tiny, 1, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: near-singular upper, transpose triggers careful solve', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 1, tiny, 0, 1, 1, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: near-singular lower, transpose triggers careful solve', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 1, 1, 0, tiny, 1, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: near-singular unit diagonal triggers careful solve', function t() {
	// Unit diagonal with large off-diagonal entries and tiny grow
	var A = new Float64Array( [ 1, 0, 0, 1e200, 1, 0, 1e200, 1e200, 1 ] );
	var x = new Float64Array( [ 1e200, 1e200, 1e200 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: zero diagonal triggers singular path', function t() {
	// Zero diagonal entry — should set scale to 0, x to unit vector
	var A = new Float64Array( [ 0, 0, 0, 1, 0, 0, 1, 1, 0 ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 0.0 );
});

test( 'dlatrs: very large x triggers xmax > BIGNUM scaling', function t() {
	// x values so large that xmax > BIGNUM triggers pre-scaling
	var A = new Float64Array( [ 1e-300, 0, 0, 0, 1e-300, 0, 0, 0, 1e-300 ] );
	var x = new Float64Array( [ 1e300, 1e300, 1e300 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: near-singular lower, non-transpose, unit diagonal triggers careful solve', function t() {
	// Unit diagonal + tiny grow in non-transpose lower
	var A = new Float64Array( [ 1, 1e200, 1e200, 0, 1, 1e200, 0, 0, 1 ] );
	var x = new Float64Array( [ 1e200, 1e200, 1e200 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: near-singular lower, transpose, unit diagonal triggers careful solve', function t() {
	var A = new Float64Array( [ 1, 1e200, 1e200, 0, 1, 1e200, 0, 0, 1 ] );
	var x = new Float64Array( [ 1e200, 1e200, 1e200 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: small diagonal with large x triggers xj > tjj*BIGNUM scaling', function t() {
	// Diagonal ~1e-300, x ~1 => xj/(diag) would overflow without scaling
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 0, tiny, 0, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: upper with off-diag, non-unit, triggers non-trivial careful solve', function t() {
	// Upper triangular with small diagonal and off-diagonal entries
	// Forces the careful path and exercises the update (j > 0) branch
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 0.5, tiny, 0, 0.3, 0.7, tiny ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: lower with off-diag, non-unit, triggers non-trivial careful solve', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0.5, 0.3, 0, tiny, 0.7, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: upper transpose with off-diag, triggers transpose careful solve', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 0.5, tiny, 0, 0.3, 0.7, tiny ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: lower transpose with off-diag, triggers transpose careful solve', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0.5, 0.3, 0, tiny, 0.7, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: large CNORM triggers tscal scaling path', function t() {
	// Off-diagonal entries > BIGNUM to trigger tscal != 1 path
	var big = 1e300;
	var A = new Float64Array( [ 1, 0, 0, big, 1, 0, big, big, 1 ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: large CNORM triggers tscal scaling, lower', function t() {
	var big = 1e300;
	var A = new Float64Array( [ 1, big, big, 0, 1, big, 0, 0, 1 ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'no-transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlatrs: small diagonal with large x, transpose', function t() {
	var tiny = 1e-300;
	var A = new Float64Array( [ tiny, 0, 0, 0, tiny, 0, 0, 0, tiny ] );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'lower', 'transpose', 'non-unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatrs: upper, transpose, unit diagonal', function t() {
	var A = new Float64Array( [ 99, 0, 0, 1, 99, 0, 1, 2, 99 ] );
	var x = new Float64Array( [ 6, 5, 4 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatrs( 'upper', 'transpose', 'unit', 'no', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 1.0 );
	// A^T * x = b: a^T is lower triangular with unit diag
	// x[0] = 6, x[1] = 5 - 1*6 = -1, x[2] = 4 - 1*6 - 2*(-1) = 0
	assertClose( x[ 0 ], 6, 1e-14, 'x[0]' );
	assertClose( x[ 1 ], -1, 1e-14, 'x[1]' );
	assertClose( x[ 2 ], 0, 1e-14, 'x[2]' );
});
