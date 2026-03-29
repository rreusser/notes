'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlapmr = require( './zlapmr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlapmr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlapmr;
