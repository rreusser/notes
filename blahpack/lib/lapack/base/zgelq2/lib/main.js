

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgelq2 = require( './zgelq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgelq2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgelq2;
