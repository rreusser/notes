/* eslint-disable max-len */
'use strict';
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlahr2 = require( './../lib/base.js' );

// FIXTURES //

var basic_n6_k1_nb2 = require( './fixtures/basic_n6_k1_nb2.json' );
var n5_k2_nb2 = require( './fixtures/n5_k2_nb2.json' );
var n8_k1_nb3 = require( './fixtures/n8_k1_nb3.json' );
var n_one = require( './fixtures/n_one.json' );

test( 'zlahr2: n6_k1_nb2', function () {
	var tc = basic_n6_k1_nb2;
	var A = new Complex128Array( 6 * 3 ); var av = reinterpret( A, 0 );
	av.set([1,0, 5,-0.5, 9,1, 13,-1, 17,0.5, 21,0, 2,1, 6,0, 10,-1, 14,0.5, 18,0, 22,1, 3,-0.5, 7,1, 11,0, 15,0, 19,-1, 23,0.5]);
	var tau = new Complex128Array( 2 ); var T = new Complex128Array( 4 ); var Y = new Complex128Array( 12 );
	zlahr2( 6, 1, 2, A, 1, 6, 0, tau, 1, 0, T, 1, 2, 0, Y, 1, 6, 0 );
	assertArrayClose( av, new Float64Array( tc.A ), 1e-13, 'A' );
	assertArrayClose( reinterpret(tau,0), new Float64Array( tc.TAU ), 1e-13, 'TAU' );
	assertArrayClose( reinterpret(T,0), new Float64Array( tc.T ), 1e-13, 'T' );
	assertArrayClose( reinterpret(Y,0), new Float64Array( tc.Y ), 1e-13, 'Y' );
});
test( 'zlahr2: n5_k2_nb2', function () {
	var tc = n5_k2_nb2;
	var A = new Complex128Array( 5 * 3 ); var av = reinterpret( A, 0 );
	av.set([1,0.5, 2,-1, 3,0, 4,1, 5,-0.5, 6,0, 7,1, 8,-1, 9,0.5, 10,0, 11,-0.5, 12,0, 13,1, 14,-1, 15,0.5]);
	var tau = new Complex128Array( 2 ); var T = new Complex128Array( 4 ); var Y = new Complex128Array( 10 );
	zlahr2( 5, 2, 2, A, 1, 5, 0, tau, 1, 0, T, 1, 2, 0, Y, 1, 5, 0 );
	assertArrayClose( av, new Float64Array( tc.A ), 1e-13, 'A' );
	assertArrayClose( reinterpret(tau,0), new Float64Array( tc.TAU ), 1e-13, 'TAU' );
	assertArrayClose( reinterpret(T,0), new Float64Array( tc.T ), 1e-13, 'T' );
	assertArrayClose( reinterpret(Y,0), new Float64Array( tc.Y ), 1e-13, 'Y' );
});
test( 'zlahr2: n8_k1_nb3', function () {
	var tc = n8_k1_nb3;
	var A = new Complex128Array( 8 * 4 ); var av = reinterpret( A, 0 );
	av.set([1,0, 2,1, 3,-1, 4,0.5, 5,0, 6,-0.5, 7,1, 8,0, 9,0.5, 10,0, 11,1, 12,-1, 13,0, 14,0.5, 15,0, 16,-0.5, 17,-1, 18,0, 19,0.5, 20,0, 21,-0.5, 22,1, 23,0, 24,0.5, 25,0, 26,-1, 27,0.5, 28,0, 29,1, 30,0, 31,-0.5, 32,1]);
	var tau = new Complex128Array( 3 ); var T = new Complex128Array( 9 ); var Y = new Complex128Array( 24 );
	zlahr2( 8, 1, 3, A, 1, 8, 0, tau, 1, 0, T, 1, 3, 0, Y, 1, 8, 0 );
	assertArrayClose( av, new Float64Array( tc.A ), 1e-13, 'A' );
	assertArrayClose( reinterpret(tau,0), new Float64Array( tc.TAU ), 1e-13, 'TAU' );
	assertArrayClose( reinterpret(T,0), new Float64Array( tc.T ), 1e-13, 'T' );
	assertArrayClose( reinterpret(Y,0), new Float64Array( tc.Y ), 1e-13, 'Y' );
});
test( 'zlahr2: n_one', function () {
	var tc = n_one;
	var A = new Complex128Array( 1 ); var av = reinterpret( A, 0 );
	av[ 0 ] = 42.0; av[ 1 ] = 3.0;
	var tau = new Complex128Array( 1 ); var T = new Complex128Array( 1 ); var Y = new Complex128Array( 1 );
	zlahr2( 1, 1, 1, A, 1, 1, 0, tau, 1, 0, T, 1, 1, 0, Y, 1, 1, 0 );
	assertArrayClose( av, new Float64Array( tc.A ), 1e-14, 'A' );
});
