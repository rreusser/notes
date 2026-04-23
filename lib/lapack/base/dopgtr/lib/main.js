
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dopgtr = require( './dopgtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dopgtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dopgtr;
