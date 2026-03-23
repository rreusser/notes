

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorg2l = require( './dorg2l.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorg2l, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorg2l;
