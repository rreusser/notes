

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zher = require( './zher.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zher, 'ndarray', ndarray );


// EXPORTS //

module.exports = zher;
