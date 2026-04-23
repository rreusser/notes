'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpttrs = require( './zpttrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpttrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpttrs;
