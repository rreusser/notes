'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlangt = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlangt.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'dlangt: max_norm_4x4', function t() {
	var tc = findCase( 'max_norm_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	var result = dlangt( 'max-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_4x4', function t() {
	var tc = findCase( 'one_norm_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	var result = dlangt( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_O alias same as one_norm', function t() {
	var tc = findCase( 'one_norm_O_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	// In JS we use 'one-norm' for both 'O' and '1'
	var result = dlangt( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_4x4', function t() {
	var tc = findCase( 'inf_norm_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	var result = dlangt( 'infinity-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_4x4', function t() {
	var tc = findCase( 'frob_norm_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	var result = dlangt( 'frobenius-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_E alias same as frobenius-norm', function t() {
	var tc = findCase( 'frob_norm_E_4x4' );
	var dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	var d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	var result = dlangt( 'frobenius-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: max_norm_n1', function t() {
	var tc = findCase( 'max_norm_n1' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( [ -7.0 ] );
	var du = new Float64Array( 0 );
	var result = dlangt( 'max-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_n1', function t() {
	var tc = findCase( 'one_norm_n1' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( [ -7.0 ] );
	var du = new Float64Array( 0 );
	var result = dlangt( 'one-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_n1', function t() {
	var tc = findCase( 'inf_norm_n1' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( [ -7.0 ] );
	var du = new Float64Array( 0 );
	var result = dlangt( 'infinity-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_n1', function t() {
	var tc = findCase( 'frob_norm_n1' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( [ -7.0 ] );
	var du = new Float64Array( 0 );
	var result = dlangt( 'frobenius-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: n_zero', function t() {
	var dl = new Float64Array( 0 );
	var d = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var result = dlangt( 'max-norm', 0, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'dlangt: max_norm_5x5', function t() {
	var tc = findCase( 'max_norm_5x5' );
	var dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	var du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = dlangt( 'max-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_5x5', function t() {
	var tc = findCase( 'one_norm_5x5' );
	var dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	var du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = dlangt( 'one-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_5x5', function t() {
	var tc = findCase( 'inf_norm_5x5' );
	var dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	var du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = dlangt( 'infinity-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_5x5', function t() {
	var tc = findCase( 'frob_norm_5x5' );
	var dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	var du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var result = dlangt( 'frobenius-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: max_norm_n2', function t() {
	var tc = findCase( 'max_norm_n2' );
	var dl = new Float64Array( [ 0.5 ] );
	var d = new Float64Array( [ 3.0, 4.0 ] );
	var du = new Float64Array( [ 1.5 ] );
	var result = dlangt( 'max-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_n2', function t() {
	var tc = findCase( 'one_norm_n2' );
	var dl = new Float64Array( [ 0.5 ] );
	var d = new Float64Array( [ 3.0, 4.0 ] );
	var du = new Float64Array( [ 1.5 ] );
	var result = dlangt( 'one-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_n2', function t() {
	var tc = findCase( 'inf_norm_n2' );
	var dl = new Float64Array( [ 0.5 ] );
	var d = new Float64Array( [ 3.0, 4.0 ] );
	var du = new Float64Array( [ 1.5 ] );
	var result = dlangt( 'infinity-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_n2', function t() {
	var tc = findCase( 'frob_norm_n2' );
	var dl = new Float64Array( [ 0.5 ] );
	var d = new Float64Array( [ 3.0, 4.0 ] );
	var du = new Float64Array( [ 1.5 ] );
	var result = dlangt( 'frobenius-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
