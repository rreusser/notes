'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dptts2 = require( './dptts2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dptts2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dptts2;
