'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpftrs = require( './zpftrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpftrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpftrs;
