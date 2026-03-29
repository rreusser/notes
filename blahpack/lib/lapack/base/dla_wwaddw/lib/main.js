'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dla_wwaddw = require( './dla_wwaddw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dla_wwaddw, 'ndarray', ndarray );


// EXPORTS //

module.exports = dla_wwaddw;
