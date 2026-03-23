'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlaswp = require( './zlaswp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaswp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaswp;
