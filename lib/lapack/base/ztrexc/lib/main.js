'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrexc = require( './ztrexc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrexc, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrexc;
