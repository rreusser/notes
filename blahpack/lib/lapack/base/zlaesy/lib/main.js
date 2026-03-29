'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaesy = require( './zlaesy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaesy, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaesy;
