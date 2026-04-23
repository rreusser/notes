'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrsen = require( './ztrsen.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrsen, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrsen;
