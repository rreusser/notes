'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpr = require( './zhpr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpr;
