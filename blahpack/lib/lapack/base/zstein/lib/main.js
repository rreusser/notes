'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zstein = require( './zstein.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zstein, 'ndarray', ndarray );


// EXPORTS //

module.exports = zstein;
