/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var dlasv2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlasv2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* CheckResult.
*
* @private
* @param {*} result - result
* @param {*} tc - tc
* @param {number} tol - tolerance
*/
function checkResult( result, tc, tol ) {
	assertClose( result.ssmin, tc.ssmin, tol, 'ssmin' );
	assertClose( result.ssmax, tc.ssmax, tol, 'ssmax' );
	assertClose( result.snr, tc.snr, tol, 'snr' );
	assertClose( result.csr, tc.csr, tol, 'csr' );
	assertClose( result.snl, tc.snl, tol, 'snl' );
	assertClose( result.csl, tc.csl, tol, 'csl' );
}

// Map test name -> input (f, g, h)
var inputs = {
	'diagonal_no_swap': [ 3.0, 0.0, 4.0 ],
	'diagonal_swap': [ 4.0, 0.0, 3.0 ],
	'identity': [ 1.0, 0.0, 1.0 ],
	'h_zero': [ 3.0, 4.0, 0.0 ],
	'all_zero': [ 0.0, 0.0, 0.0 ],
	'f_zero': [ 0.0, 5.0, 3.0 ],
	'general_1_2_3': [ 1.0, 2.0, 3.0 ],
	'negative_vals': [ -2.0, 1.0, -3.0 ],
	'very_large_g': [ 1.0, 1e20, 1.0 ],
	'very_large_g_ha_gt_1': [ 1.0, 1e20, 2.0 ],
	'swap_general': [ 1.0, 2.0, 5.0 ],
	'neg_f_pos_h': [ -3.0, 4.0, 5.0 ],
	'neg_g': [ 2.0, -3.0, 1.0 ],
	'f_g_zero': [ 0.0, 0.0, 5.0 ],
	'h_g_zero': [ 5.0, 0.0, 0.0 ],
	'pmax_2': [ 0.5, 10.0, 0.5 ],
	'ha_very_small': [ 10.0, 1.0, 1e-320 ],
	'mm_zero_l_nonzero': [ 2.0, 1e-300, 1.0 ],
	'mm_zero_l_zero': [ 2.0, 1e-300, 2.0 ],
	'mm_zero_l_zero_neg': [ -2.0, -1e-300, 2.0 ],
	'all_negative': [ -1.0, -2.0, -3.0 ],
	'swap_very_large_g': [ 1.0, 1e20, 5.0 ],
	'f_h_zero_g_nonzero': [ 0.0, 1.0, 0.0 ],
	'equal_diagonal': [ 1.0, 1.0, 1.0 ],
	'gasmal_false_ha_gt_1': [ 2.0, 1e20, 100.0 ]
};


// TESTS //

test( 'dlasv2: diagonal_no_swap (g=0, h>f, swap triggers)', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'diagonal_no_swap' );
	inp = inputs[ 'diagonal_no_swap' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: diagonal_swap (g=0, f>h, no swap)', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'diagonal_swap' );
	inp = inputs[ 'diagonal_swap' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: identity matrix', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'identity' );
	inp = inputs[ 'identity' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: h_zero [3 4; 0 0]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'h_zero' );
	inp = inputs[ 'h_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: all zeros', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'all_zero' );
	inp = inputs[ 'all_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: f_zero [0 5; 0 3]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'f_zero' );
	inp = inputs[ 'f_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: general [1 2; 0 3]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'general_1_2_3' );
	inp = inputs[ 'general_1_2_3' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: negative values [-2 1; 0 -3]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'negative_vals' );
	inp = inputs[ 'negative_vals' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: very large g (gasmal=false path)', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'very_large_g' );
	inp = inputs[ 'very_large_g' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: very large g with ha>1', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'very_large_g_ha_gt_1' );
	inp = inputs[ 'very_large_g_ha_gt_1' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: swap + general [1 2; 0 5]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'swap_general' );
	inp = inputs[ 'swap_general' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: negative f, positive h [-3 4; 0 5]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'neg_f_pos_h' );
	inp = inputs[ 'neg_f_pos_h' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: negative g [2 -3; 0 1]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'neg_g' );
	inp = inputs[ 'neg_g' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: f=0, g=0 [0 0; 0 5]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'f_g_zero' );
	inp = inputs[ 'f_g_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: h=0, g=0 [5 0; 0 0]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'h_g_zero' );
	inp = inputs[ 'h_g_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: pmax=2 path [0.5 10; 0 0.5]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'pmax_2' );
	inp = inputs[ 'pmax_2' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: ha very small (d=fa, l=1)', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'ha_very_small' );
	inp = inputs[ 'ha_very_small' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: mm=0, l nonzero path', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'mm_zero_l_nonzero' );
	inp = inputs[ 'mm_zero_l_nonzero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: mm=0, l=0 path (equal diagonal, tiny g)', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'mm_zero_l_zero' );
	inp = inputs[ 'mm_zero_l_zero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: mm=0, l=0, negative ft and gt', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'mm_zero_l_zero_neg' );
	inp = inputs[ 'mm_zero_l_zero_neg' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: all negative [-1 -2; 0 -3]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'all_negative' );
	inp = inputs[ 'all_negative' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: swap + very large g', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'swap_very_large_g' );
	inp = inputs[ 'swap_very_large_g' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: f=0, h=0, g nonzero [0 1; 0 0]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'f_h_zero_g_nonzero' );
	inp = inputs[ 'f_h_zero_g_nonzero' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: equal diagonal [1 1; 0 1]', function t() {
	var result;
	var inp;
	var tc;

	tc = findCase( 'equal_diagonal' );
	inp = inputs[ 'equal_diagonal' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasv2: gasmal=false with ha>1 after swap [2 1e20; 0 100]', function t() { // eslint-disable-line max-len
	var result;
	var inp;
	var tc;

	tc = findCase( 'gasmal_false_ha_gt_1' );
	inp = inputs[ 'gasmal_false_ha_gt_1' ];
	result = dlasv2( inp[0], inp[1], inp[2] );
	checkResult( result, tc, 1e-14 );
});
