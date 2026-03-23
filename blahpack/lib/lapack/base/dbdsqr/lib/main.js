

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dbdsqr = require( './dbdsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dbdsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dbdsqr;
