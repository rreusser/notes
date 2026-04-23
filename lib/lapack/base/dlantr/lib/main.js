'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlantr = require( './dlantr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlantr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlantr;
