/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgelqf = require( './../lib/dgelqf.js' );


// TESTS //

test( 'dgelqf is a function', function t() {
	assert.strictEqual( typeof dgelqf, 'function', 'is a function' );
});

test( 'dgelqf has expected arity', function t() {
	assert.strictEqual( dgelqf.length, 9, 'has expected arity' );
});

test( 'dgelqf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgelqf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgelqf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgelqf( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgelqf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgelqf( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
