'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgels = require( './zgels.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgels, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgels;
