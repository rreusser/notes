/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaneg = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlaneg.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

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
		return {
			'N': 1,
			'd': new Float64Array( [ 2.0 ] ),
			'lld': new Float64Array( 0 ),
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 1
		}; // eslint-disable-line max-len
	case 'n1_sigma_above':
		return {
			'N': 1,
			'd': new Float64Array( [ 2.0 ] ),
			'lld': new Float64Array( 0 ),
			'sigma': 5.0,
			'pivmin': 1e-30,
			'r': 1
		}; // eslint-disable-line max-len
	case 'n5_sigma_zero_r3':
		return {
			'N': 5,
			'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ),
			'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ),
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 3
		}; // eslint-disable-line max-len
	case 'n5_sigma_zero_r5':
		return {
			'N': 5,
			'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ),
			'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ),
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 5
		}; // eslint-disable-line max-len
	case 'n5_sigma_zero_r1':
		return {
			'N': 5,
			'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ),
			'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ),
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 1
		}; // eslint-disable-line max-len
	case 'n5_sigma_large_r3':
		return {
			'N': 5,
			'd': new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] ),
			'lld': new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] ),
			'sigma': 10.0,
			'pivmin': 1e-30,
			'r': 3
		}; // eslint-disable-line max-len
	case 'n4_mixed_r2':
		return {
			'N': 4,
			'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ),
			'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ),
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 2
		}; // eslint-disable-line max-len
	case 'n4_mixed_neg_sigma':
		return {
			'N': 4,
			'd': new Float64Array( [ -1.0, 2.0, -3.0, 4.0 ] ),
			'lld': new Float64Array( [ 0.25, 0.25, 0.25 ] ),
			'sigma': -1.0,
			'pivmin': 1e-30,
			'r': 2
		}; // eslint-disable-line max-len
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
			return {
				'N': 150,
				'd': d,
				'lld': lld,
				'sigma': 0.0,
				'pivmin': 1e-30,
				'r': 75
			}; // eslint-disable-line max-len
		}
		if ( name === 'n150_sigma_50' ) {
			return {
				'N': 150,
				'd': d,
				'lld': lld,
				'sigma': 50.5,
				'pivmin': 1e-30,
				'r': 75
			}; // eslint-disable-line max-len
		}
		if ( name === 'n150_r_n' ) {
			return {
				'N': 150,
				'd': d,
				'lld': lld,
				'sigma': 0.0,
				'pivmin': 1e-30,
				'r': 150
			}; // eslint-disable-line max-len
		}
		return {
			'N': 150,
			'd': d,
			'lld': lld,
			'sigma': 0.0,
			'pivmin': 1e-30,
			'r': 1
		}; // eslint-disable-line max-len
	case 'n2_r1':
		return {
			'N': 2,
			'd': new Float64Array( [ 1.0, 4.0 ] ),
			'lld': new Float64Array( [ 1.0 ] ),
			'sigma': 0.5,
			'pivmin': 1e-30,
			'r': 1
		}; // eslint-disable-line max-len
	case 'n2_r2':
		return {
			'N': 2,
			'd': new Float64Array( [ 1.0, 4.0 ] ),
			'lld': new Float64Array( [ 1.0 ] ),
			'sigma': 0.5,
			'pivmin': 1e-30,
			'r': 2
		}; // eslint-disable-line max-len
	default:
		throw new Error( format( 'unknown case: `%s`.', name ) );
	}
}

/**
* RunCase.
*
* @private
* @param {string} name - test case name
*/
function runCase( name ) {
	var inputs = buildCase( name );
	var result = dlaneg( inputs.N, inputs.d, 1, 0, inputs.lld, 1, 0, inputs.sigma, inputs.pivmin, inputs.r ); // eslint-disable-line max-len
	var tc = findCase( name );
	assert.equal( result, tc.negcnt, format( '%s: expected %d, got %d', name, tc.negcnt, result ) ); // eslint-disable-line max-len
}


// TESTS //

fixture.forEach( function each( tc ) {
	test( 'dlaneg: ' + tc.name, function t() {
		runCase( tc.name );
	});
});

test( 'dlaneg: non-unit stride on d and lld', function t() {
	var lbase;
	var base;
	var lld;
	var r1;
	var r2;
	var N;
	var d;
	var i;

	base = [ 4.0, 3.0, 2.0, 1.0, 5.0 ];
	lbase = [ 0.5, 0.5, 0.5, 0.5 ];
	N = 5;
	d = new Float64Array( N * 2 );
	lld = new Float64Array( ( N - 1 ) * 3 );
	for ( i = 0; i < N; i++ ) {
		d[ i * 2 ] = base[ i ];
	}
	for ( i = 0; i < N - 1; i++ ) {
		lld[ i * 3 ] = lbase[ i ];
	}
	r1 = dlaneg( N, d, 2, 0, lld, 3, 0, 10.0, 1e-30, 3 );
	r2 = dlaneg( N, new Float64Array( base ), 1, 0, new Float64Array( lbase ), 1, 0, 10.0, 1e-30, 3 ); // eslint-disable-line max-len
	assert.equal( r1, r2, 'strided result matches unit-stride result' );
});

test( 'dlaneg: non-zero offsets on d and lld', function t() {
	var lld;
	var d;
	var r;

	d = new Float64Array( [ 99, 99, 4.0, 3.0, 2.0, 1.0, 5.0 ] );
	lld = new Float64Array( [ 77, 0.5, 0.5, 0.5, 0.5 ] );
	r = dlaneg( 5, d, 1, 2, lld, 1, 1, 10.0, 1e-30, 3 );
	assert.equal( r, 5 );
});

test( 'dlaneg: NaN in input activates slow path', function t() {
	var lld;
	var d;
	var r;

	d = new Float64Array( [ 1.0, NaN, 3.0, 4.0, 5.0 ] );
	lld = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );
	r = dlaneg( 5, d, 1, 0, lld, 1, 0, 0.0, 1e-30, 3 );
	assert.equal( typeof r, 'number' );
	assert.ok( Number.isInteger( r ) );
});

test( 'dlaneg: NaN in lower sweep activates slow path', function t() {
	var lld;
	var d;
	var r;

	d = new Float64Array( [ 1.0, 2.0, 3.0, NaN, 5.0 ] );
	lld = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );
	r = dlaneg( 5, d, 1, 0, lld, 1, 0, 0.0, 1e-30, 2 );
	assert.equal( typeof r, 'number' );
	assert.ok( Number.isInteger( r ) );
});
