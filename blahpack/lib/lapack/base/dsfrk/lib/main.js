'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsfrk = require( './dsfrk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsfrk, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsfrk;
