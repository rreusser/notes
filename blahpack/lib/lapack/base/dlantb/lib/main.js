'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlantb = require( './dlantb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlantb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlantb;
