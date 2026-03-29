/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpr2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhpr2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}


// TESTS //

test( 'zhpr2 is a function', function t() {
	assert.strictEqual( typeof zhpr2, 'function' );
});

test( 'zhpr2: upper triangle, N=3, alpha=(1,0)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'upper_basic' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: lower triangle, N=3, alpha=(1,0)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'lower_basic' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, -1.0, 3.0, 2.0, 4.0, 0.0, 2.0, -1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'lower', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: complex alpha=(2,1)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'complex_alpha' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 2.0, 1.0 );
	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: alpha=0 is a no-op', function t() {
	var expected;
	var alpha;
	var ap;
	var rv;
	var x;
	var y;

	expected = [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ];
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 1.5, -0.5, 2.5, 0.0 ] );
	alpha = new Complex128( 0.0, 0.0 );
	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, expected, 1e-14, 'AP' );
});

test( 'zhpr2: N=0 quick return', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'n_zero' );
	ap = new Complex128Array( [ 99.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5 ] );
	y = new Complex128Array( [ 0.5, 1.0 ] );
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'upper', 0, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: N=1 scalar case', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'scalar' );
	ap = new Complex128Array( [ 3.0, 0.0 ] );
	x = new Complex128Array( [ 2.0, 1.0 ] );
	y = new Complex128Array( [ 1.0, -0.5 ] );
	alpha = new Complex128( 1.0, 0.5 );
	zhpr2( 'upper', 1, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: non-unit stride (incx=2, incy=2)', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'stride_2' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 3.0, 1.0 ] ); // eslint-disable-line max-len
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 1.5, -0.5, 0.0, 0.0, 2.5, 0.0 ] ); // eslint-disable-line max-len
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'upper', 3, alpha, x, 2, 0, y, 2, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: zero elements in x and y', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'zero_elements' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, 1.0, 4.0, 0.0, 3.0, -2.0, 2.0, 1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});

test( 'zhpr2: lower triangle with zero elements in x and y', function t() {
	var alpha;
	var ap;
	var rv;
	var x;
	var y;

	ap = new Complex128Array( [ 2.0, 0.5, 1.0, -1.0, 3.0, 2.0, 4.0, 0.3, 2.0, -1.0, 5.0, 0.1 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 2.5, 0.0 ] );
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'lower', 3, alpha, x, 1, 0, y, 1, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assert.ok( Math.abs( rv[0] - 4.0 ) < 1e-14, 'ap[0,0] real' );
	assert.ok( Math.abs( rv[1] - 0.0 ) < 1e-14, 'ap[0,0] imag' );
	assert.ok( Math.abs( rv[7] - 0.0 ) < 1e-14, 'ap[1,1] imag forced to 0' );
	assert.ok( Math.abs( rv[11] - 0.0 ) < 1e-14, 'ap[2,2] imag' );
});

test( 'zhpr2: lower triangle with non-unit stride', function t() {
	var alpha;
	var tc;
	var ap;
	var rv;
	var x;
	var y;

	tc = findCase( 'lower_stride_2' );
	ap = new Complex128Array( [ 2.0, 0.0, 1.0, -1.0, 3.0, 2.0, 4.0, 0.0, 2.0, -1.0, 5.0, 0.0 ] ); // eslint-disable-line max-len
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 3.0, 1.0 ] ); // eslint-disable-line max-len
	y = new Complex128Array( [ 0.5, 1.0, 0.0, 0.0, 1.5, -0.5, 0.0, 0.0, 2.5, 0.0 ] ); // eslint-disable-line max-len
	alpha = new Complex128( 1.0, 0.0 );
	zhpr2( 'lower', 3, alpha, x, 2, 0, y, 2, 0, ap, 1, 0 );
	rv = reinterpret( ap, 0 );
	assertArrayClose( rv, tc.AP, 1e-14, 'AP' );
});
