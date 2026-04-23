'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlatbs = require( './zlatbs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlatbs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlatbs;
