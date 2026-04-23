
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsdot = require( './dsdot.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsdot, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsdot;
