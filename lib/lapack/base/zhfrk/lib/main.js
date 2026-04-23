
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhfrk = require( './zhfrk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhfrk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhfrk;
