/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaneg = require( './../lib/dlaneg.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var fixture = readFileSync( path.join( fixtureDir, 'dlaneg.jsonl' ), 'utf8' ) // eslint-disable-line node/no-sync
	.trim()
	.split( '\n' )
	.map( function parse( line ) {
		return JSON.parse( line );
	});


// FUNCTIONS //

/**
* Constructs the inputs for a named test case.
*
* @private
* @param {string} name - test case name
* @throws {Error} if the case name is unknown
* @returns {Object} test case inputs
*/
function buildCase( name ) {
	var lld;
	var d;
	var i;
	switch ( name ) {
	case 'n1_sigma_below':
		return { 'N': 1, 'd': new Float64Array( [ 2.0 ] ), 'lld': new Float64Array( 0 ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n1_sigma_above':
		return { 'N': 1, 'd': new Float64Array( [ 2.0 ] ), 'lld': new Float64Array( 0 ), 'sigma': 5.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n5_sigma_zero_r3':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 3 };
	case 'n5_sigma_zero_r5':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 5 };
	case 'n5_sigma_zero_r1':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n5_sigma_large_r3':
		return { 'N': 5, 'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ), 'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ), 'sigma': 10.0, 'pivmin': 1e-30, 'r': 3 };
	case 'n4_mixed_r2':
		return { 'N': 4, 'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ), 'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ), 'sigma': 0.0, 'pivmin': 1e-30, 'r': 2 };
	case 'n4_mixed_neg_sigma':
		return { 'N': 4, 'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ), 'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ), 'sigma': -1.0, 'pivmin': 1e-30, 'r': 2 };
	case 'n150_sigma_zero':
	case 'n150_sigma_50':
	case 'n150_r_n':
	case 'n150_r_1':
		d = new Float64Array( 150 );
		lld = new Float64Array( 149 );
		for ( i = 0; i < 150; i++ ) {
			d[ i ] = i + 1;
		}
		for ( i = 0; i < 149; i++ ) {
			lld[ i ] = 0.1;
		}
		if ( name === 'n150_sigma_zero' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 75 };
		}
		if ( name === 'n150_sigma_50' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 50.5, 'pivmin': 1e-30, 'r': 75 };
		}
		if ( name === 'n150_r_n' ) {
			return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 150 };
		}
		return { 'N': 150, 'd': d, 'lld': lld, 'sigma': 0.0, 'pivmin': 1e-30, 'r': 1 };
	case 'n2_r1':
		return { 'N': 2, 'd': new Float64Array( [ 1.0, 4.0 ] ), 'lld': new Float64Array( [ 1.0 ] ), 'sigma': 0.5, 'pivmin': 1e-30, 'r': 1 };
	case 'n2_r2':
		return { 'N': 2, 'd': new Float64Array( [ 1.0, 4.0 ] ), 'lld': new Float64Array( [ 1.0 ] ), 'sigma': 0.5, 'pivmin': 1e-30, 'r': 2 };
	default:
		throw new Error( format( 'unknown case: `%s`.', name ) );
	}
}


// TESTS //

test( 'dlaneg is a function', function t() {
	assert.strictEqual( typeof dlaneg, 'function', 'is a function' );
});

test( 'dlaneg has expected arity', function t() {
	assert.strictEqual( dlaneg.length, 8, 'has expected arity' );
});

test( 'dlaneg throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaneg( -1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 0.0, 0.0, 1 );
	}, RangeError );
});

// Every fixture case is executed through the 8-argument main entry point
// with exact integer comparison against the Fortran reference `negcnt`.
fixture.forEach( function each( tc ) {
	test( 'dlaneg (main): ' + tc.name, function t() {
		var inputs = buildCase( tc.name );
		var result = dlaneg(
			inputs.N,
			inputs.d, 1,
			inputs.lld, 1,
			inputs.sigma, inputs.pivmin, inputs.r
		);
		assert.equal( result, tc.negcnt, format( '%s: expected %d, got %d', tc.name, tc.negcnt, result ) ); // eslint-disable-line max-len
	});
});

test( 'dlaneg (main): handles negative strideD', function t() {
	var d = new Float64Array( [ 5.0, 1.0, 2.0, 3.0, 4.0 ] );
	var lld = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );
	// Reverse iteration: with strideD = -1 and stride2offset, the wrapper
	// walks d from the end to the beginning.
	var result = dlaneg( 5, d, -1, lld, 1, 10.0, 1e-30, 3 );
	assert.equal( typeof result, 'number' );
	assert.ok( Number.isInteger( result ), 'returns an integer Sturm count' );
});
