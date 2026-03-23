

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zher2k = require( './zher2k.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zher2k, 'ndarray', ndarray );


// EXPORTS //

module.exports = zher2k;
