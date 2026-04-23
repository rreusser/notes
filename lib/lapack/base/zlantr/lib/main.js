'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlantr = require( './zlantr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlantr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlantr;
