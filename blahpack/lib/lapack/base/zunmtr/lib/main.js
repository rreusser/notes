'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunmtr = require( './zunmtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmtr;
