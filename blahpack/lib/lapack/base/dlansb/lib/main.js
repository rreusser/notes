'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlansb = require( './dlansb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlansb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlansb;
