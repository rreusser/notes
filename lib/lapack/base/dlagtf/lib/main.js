'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlagtf = require( './dlagtf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlagtf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlagtf;
