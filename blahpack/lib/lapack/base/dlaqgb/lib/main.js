
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqgb = require( './dlaqgb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqgb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqgb;
