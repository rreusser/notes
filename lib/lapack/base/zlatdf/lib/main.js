
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlatdf = require( './zlatdf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlatdf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlatdf;
