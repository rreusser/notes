'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlartv = require( './dlartv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlartv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlartv;
