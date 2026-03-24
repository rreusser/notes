

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlatbs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlatbs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Creates banded storage (KD+1 x N) in column-major from entries.
* entries is array of [row0based, col, value].
*/
function bandedMatrix( kdp1, n, entries ) {
	var ab = new Float64Array( kdp1 * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		ab[ (entries[i][1] * kdp1) + entries[i][0] ] = entries[i][2];
	}
	return ab;
}


// TESTS //

test( 'dlatbs: upper_N_nonunit', function t() {
	var tc = findCase( 'upper_N_nonunit' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0], [2, 1, 3.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 5.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_nonunit', function t() {
	var tc = findCase( 'lower_N_nonunit' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 3.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 5.0], [1, 2, 3.0],
		[0, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'lower', 'no-transpose', 'non-unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit', function t() {
	var tc = findCase( 'upper_T_nonunit' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0], [2, 1, 3.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 5.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'upper', 'transpose', 'non-unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit', function t() {
	var tc = findCase( 'lower_T_nonunit' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 3.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 5.0], [1, 2, 3.0],
		[0, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'lower', 'transpose', 'non-unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_N_unit', function t() {
	var tc = findCase( 'upper_N_unit' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0], [2, 1, 99.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 99.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 99.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'upper', 'no-transpose', 'unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit', function t() {
	var tc = findCase( 'lower_N_unit' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 99.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 99.0], [1, 2, 3.0],
		[0, 3, 99.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'lower', 'no-transpose', 'unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var ab = new Float64Array( 1 );
	var x = new Float64Array( 1 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 0, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
});

test( 'dlatbs: n_one', function t() {
	var tc = findCase( 'n_one' );
	var ab = new Float64Array([ 5.0 ]);
	var x = new Float64Array([ 10.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 1, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
});

test( 'dlatbs: normin_Y', function t() {
	var tc = findCase( 'normin_Y' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0], [2, 1, 3.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 5.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'Y', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_kd1', function t() {
	var tc = findCase( 'upper_kd1' );
	var ab = bandedMatrix( 2, 4, [
		[1, 0, 3.0],
		[0, 1, 1.0], [1, 1, 4.0],
		[0, 2, 2.0], [1, 2, 5.0],
		[0, 3, 1.0], [1, 3, 6.0]
	]);
	var x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_unit', function t() {
	var tc = findCase( 'upper_T_unit' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0], [2, 1, 99.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 99.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 99.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'upper', 'transpose', 'unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_unit', function t() {
	var tc = findCase( 'lower_T_unit' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 99.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 99.0], [1, 2, 3.0],
		[0, 3, 99.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'lower', 'transpose', 'unit', 'N', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit_normin_Y', function t() {
	var tc = findCase( 'lower_T_nonunit_normin_Y' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 3.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 5.0], [1, 2, 3.0],
		[0, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	var info = dlatbs( 'lower', 'transpose', 'non-unit', 'Y', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit_normin_Y', function t() {
	var tc = findCase( 'upper_T_nonunit_normin_Y' );
	var ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0], [2, 1, 3.0],
		[0, 2, 1.0], [1, 2, 1.0], [2, 2, 5.0],
		[0, 3, 2.0], [1, 3, 3.0], [2, 3, 6.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	var info = dlatbs( 'upper', 'transpose', 'non-unit', 'Y', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit_normin_Y', function t() {
	var tc = findCase( 'lower_N_unit_normin_Y' );
	var ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0], [1, 0, 2.0], [2, 0, 1.0],
		[0, 1, 99.0], [1, 1, 1.0], [2, 1, 2.0],
		[0, 2, 99.0], [1, 2, 3.0],
		[0, 3, 99.0]
	]);
	var x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	var info = dlatbs( 'lower', 'no-transpose', 'unit', 'Y', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_kd1', function t() {
	var tc = findCase( 'lower_T_kd1' );
	var ab = bandedMatrix( 2, 4, [
		[0, 0, 3.0], [1, 0, 1.0],
		[0, 1, 4.0], [1, 1, 2.0],
		[0, 2, 5.0], [1, 2, 1.0],
		[0, 3, 6.0]
	]);
	var x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info = dlatbs( 'lower', 'transpose', 'non-unit', 'N', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: singular_upper (zero diagonal)', function t() {
	var tc = findCase( 'singular_upper' );
	var ab = bandedMatrix( 2, 3, [
		[1, 0, 2.0],
		[0, 1, 1.0], [1, 1, 0.0],
		[0, 2, 1.0], [1, 2, 3.0]
	]);
	var x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: near_singular_upper (very small diagonal)', function t() {
	var tc = findCase( 'near_singular_upper' );
	var ab = bandedMatrix( 2, 3, [
		[1, 0, 1.0],
		[0, 1, 1.0], [1, 1, 1e-300],
		[0, 2, 1.0], [1, 2, 1.0]
	]);
	var x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'N', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from(x), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from(cnorm), tc.cnorm, 1e-14, 'cnorm' );
});
