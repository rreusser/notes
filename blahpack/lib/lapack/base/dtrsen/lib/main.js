'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrsen = require( './dtrsen.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrsen, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrsen;
