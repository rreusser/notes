
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarzt = require( './zlarzt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarzt, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarzt;
