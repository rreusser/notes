

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zher2 = require( './zher2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zher2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zher2;
