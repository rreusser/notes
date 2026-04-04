/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhptrf = require( '../../zhptrf/lib/base.js' );
var zhpsvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhpsvx.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// VARIABLES //

// Upper packed 3x3 Hermitian: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3)
var AP_UPPER_3 = [ // eslint-disable-line no-unused-vars
	4.0,
	0.0,
	1.0,
	2.0,
	5.0,
	0.0,
	2.0,
	-1.0,
	3.0,
	1.0,
	6.0,
	0.0
];

// Lower packed 3x3 Hermitian
var AP_LOWER_3 = [ // eslint-disable-line no-unused-vars
	4.0,
	0.0,
	1.0,
	-2.0,
	2.0,
	1.0,
	5.0,
	0.0,
	3.0,
	-1.0,
	6.0,
	0.0
];

// B for tests 1,3,8 (1 RHS)
var bData1 = [ 1.0, 0.0, 0.0, 1.0, 1.0, -1.0 ]; // eslint-disable-line no-unused-vars

// B for multi-rhs (2 RHS), column-major, N*nrhs = 3*2 = 6 complex elements
var bData2 = [ // eslint-disable-line no-unused-vars
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
	-1.0,
	2.0,
	1.0,
	1.0,
	-1.0,
	0.0,
	2.0
];

// Upper packed 4x4 well-conditioned Hermitian matrix
var AP_UPPER_4 = [
	4.0,
	0.0,
	0.5,
	0.3,
	5.0,
	0.0,
	0.3,
	-0.2,
	0.4,
	0.1,
	6.0,
	0.0,
	0.2,
	0.1,
	0.1,
	-0.3,
	0.5,
	0.2,
	7.0,
	0.0
];

// B for 4x4 pivot test (1 RHS)
var bData4 = [ 1.0, 0.0, 0.0, 1.0, 2.0, -1.0, 1.0, 1.0 ];


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
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
* @param {Array} actual - actual array
* @param {Array} expected - expected array
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
* Converts a typed array to a plain Array.
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

/**
* Build a Complex128Array from a flat array of interleaved doubles.
*
* @private
* @param {Array} flatDoubles - interleaved real/imag values
* @param {integer} nc - number of complex elements
* @returns {Complex128Array} complex array
*/
function buildComplex( flatDoubles, nc ) {
	var out = new Complex128Array( nc );
	var ov = reinterpret( out, 0 );
	var i;
	for ( i = 0; i < 2 * nc; i++ ) {
		ov[ i ] = flatDoubles[ i ];
	}
	return out;
}

/**
* Helper: call zhpsvx with standard arguments.
*
* @private
* @param {string} fact - 'not-factored' or 'factored'
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order
* @param {NonNegativeInteger} nrhs - right-hand sides
* @param {Complex128Array} AP - original packed Hermitian matrix
* @param {Complex128Array} AFP - factored packed matrix (input if factored)
* @param {Int32Array} IPIV - pivot indices (input if factored)
* @param {Complex128Array} B - RHS matrix (col-major, N-by-nrhs)
* @returns {Object} result with info, x, rcond, ferr, berr, afp, ipiv
*/
function callZhpsvx( fact, uplo, N, nrhs, AP, AFP, IPIV, B ) {
	var rcond = new Float64Array( 1 );
	var RWORK = new Float64Array( Math.max( 1, N ) );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var WORK = new Complex128Array( Math.max( 1, 2 * N ) );
	var info;
	var X = new Complex128Array( N * nrhs );

	info = zhpsvx( fact, uplo, N, nrhs, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, N, 0, X, 1, N, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len

	return {
		'info': info,
		'x': reinterpret( X, 0 ),
		'rcond': rcond[ 0 ],
		'ferr': FERR,
		'berr': BERR,
		'afp': reinterpret( AFP, 0 ),
		'ipiv': IPIV
	};
}


// TESTS //

test( 'zhpsvx: fact_n_upper', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'fact_n_upper' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( tc.AP, nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( tc.n );
	B = buildComplex( tc.b, tc.n * tc.nrhs );
	res = callZhpsvx( 'not-factored', 'upper', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
	assertArrayClose( toArray( res.afp ), tc.afp, 1e-14, 'afp' );
});

test( 'zhpsvx: fact_n_lower', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'fact_n_lower' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( tc.AP, nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( tc.n );
	B = buildComplex( tc.b, tc.n * tc.nrhs );
	res = callZhpsvx( 'not-factored', 'lower', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
	assertArrayClose( toArray( res.afp ), tc.afp, 1e-14, 'afp' );
});

test( 'zhpsvx: fact_f_upper', function t() {
	var IPIV;
	var afpv;
	var apcv;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;
	var i;

	tc = findCase( 'fact_f_upper' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( AP_UPPER_3, nn );
	AFP = new Complex128Array( nn );
	afpv = reinterpret( AFP, 0 );
	apcv = reinterpret( AP, 0 );
	for ( i = 0; i < 2 * nn; i++ ) {
		afpv[ i ] = apcv[ i ];
	}
	IPIV = new Int32Array( tc.n );
	zhptrf( 'upper', tc.n, AFP, 1, 0, IPIV, 1, 0 );
	B = buildComplex( bData1, tc.n * tc.nrhs );
	res = callZhpsvx( 'factored', 'upper', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
});

test( 'zhpsvx: fact_f_lower', function t() {
	var IPIV;
	var afpv;
	var apcv;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;
	var j;

	tc = findCase( 'fact_f_lower' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( AP_LOWER_3, nn );
	AFP = new Complex128Array( nn );
	afpv = reinterpret( AFP, 0 );
	apcv = reinterpret( AP, 0 );
	for ( j = 0; j < 2 * nn; j++ ) {
		afpv[ j ] = apcv[ j ];
	}
	IPIV = new Int32Array( tc.n );
	zhptrf( 'lower', tc.n, AFP, 1, 0, IPIV, 1, 0 );
	B = buildComplex( bData1, tc.n * tc.nrhs );
	res = callZhpsvx( 'factored', 'lower', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
});

test( 'zhpsvx: n_zero', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var B;

	tc = findCase( 'n_zero' );
	AP = new Complex128Array( 1 );
	AFP = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );
	res = callZhpsvx( 'not-factored', 'upper', 0, 1, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
});

test( 'zhpsvx: n_one_upper', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var B;

	tc = findCase( 'n_one_upper' );
	AP = buildComplex( [ 4.0, 0.0 ], 1 );
	AFP = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = buildComplex( [ 8.0, 4.0 ], 1 );
	res = callZhpsvx( 'not-factored', 'upper', 1, 1, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
});

test( 'zhpsvx: singular', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'singular' );
	nn = ( 3 * 4 ) / 2;
	AP = buildComplex([
		4.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		6.0,
		0.0
	], nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( 3 );
	B = buildComplex( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ], 3 );
	res = callZhpsvx( 'not-factored', 'upper', 3, 1, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.rcond, tc.rcond, 'rcond' );
});

test( 'zhpsvx: multi_rhs_upper', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'multi_rhs_upper' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( AP_UPPER_3, nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( tc.n );
	B = buildComplex( bData2, tc.n * tc.nrhs );
	res = callZhpsvx( 'not-factored', 'upper', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
	assertArrayClose( toArray( res.afp ), tc.afp, 1e-14, 'afp' );
});

test( 'zhpsvx: multi_rhs_lower', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'multi_rhs_lower' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( AP_LOWER_3, nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( tc.n );
	B = buildComplex( bData2, tc.n * tc.nrhs );
	res = callZhpsvx( 'not-factored', 'lower', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
	assertArrayClose( toArray( res.afp ), tc.afp, 1e-14, 'afp' );
});

test( 'zhpsvx: upper_4x4', function t() {
	var IPIV;
	var AFP;
	var res;
	var tc;
	var AP;
	var nn;
	var B;

	tc = findCase( 'upper_4x4' );
	nn = ( tc.n * ( tc.n + 1 ) ) / 2;
	AP = buildComplex( AP_UPPER_4, nn );
	AFP = new Complex128Array( nn );
	IPIV = new Int32Array( tc.n );
	B = buildComplex( bData4, tc.n * tc.nrhs );
	res = callZhpsvx( 'not-factored', 'upper', tc.n, tc.nrhs, AP, AFP, IPIV, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( toArray( res.x ), tc.x, 1e-14, 'x' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( toArray( res.ferr ), tc.ferr, 1e-10, 'ferr' );
	assertArrayClose( toArray( res.berr ), tc.berr, 1e-10, 'berr' );
	assertArrayClose( toArray( res.afp ), tc.afp, 1e-14, 'afp' );
});
