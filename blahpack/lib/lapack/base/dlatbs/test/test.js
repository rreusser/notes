/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlatbs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlatbs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
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

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'dlatbs: upper_N_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_N_nonunit' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_N_nonunit' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'no-transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_T_nonunit' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_T_nonunit' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_N_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_N_unit' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0],
		[2, 1, 99.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 99.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_N_unit' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'no-transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: n_zero', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'n_zero' );
	ab = new Float64Array( 1 );
	x = new Float64Array( 1 );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 1 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 0, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
});

test( 'dlatbs: n_one', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'n_one' );
	ab = new Float64Array([ 5.0 ]);
	x = new Float64Array([ 10.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 1 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 1, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
});

test( 'dlatbs: normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'normin_Y' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_kd1', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_kd1' );
	ab = bandedMatrix( 2, 4, [
		[1, 0, 3.0],
		[0, 1, 1.0],
		[1, 1, 4.0],
		[0, 2, 2.0],
		[1, 2, 5.0],
		[0, 3, 1.0],
		[1, 3, 6.0]
	]);
	x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_T_unit' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0],
		[2, 1, 99.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 99.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_T_unit' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_T_nonunit_normin_Y' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'upper_T_nonunit_normin_Y' );
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	info = dlatbs( 'upper', 'transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_N_unit_normin_Y' );
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	info = dlatbs( 'lower', 'no-transpose', 'unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_kd1', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'lower_T_kd1' );
	ab = bandedMatrix( 2, 4, [
		[0, 0, 3.0],
		[1, 0, 1.0],
		[0, 1, 4.0],
		[1, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 1.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'no', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: singular_upper (zero diagonal)', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'singular_upper' );
	ab = bandedMatrix( 2, 3, [
		[1, 0, 2.0],
		[0, 1, 1.0],
		[1, 1, 0.0],
		[0, 2, 1.0],
		[1, 2, 3.0]
	]);
	x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: near_singular_upper (very small diagonal)', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = findCase( 'near_singular_upper' );
	ab = bandedMatrix( 2, 3, [
		[1, 0, 1.0],
		[0, 1, 1.0],
		[1, 1, 1e-300],
		[0, 2, 1.0],
		[1, 2, 1.0]
	]);
	x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});
