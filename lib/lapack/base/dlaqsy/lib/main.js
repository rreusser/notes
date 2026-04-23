'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaqsy = require( './dlaqsy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaqsy, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaqsy;
