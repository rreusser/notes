

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zhgeqz = require( './zhgeqz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhgeqz, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhgeqz;
