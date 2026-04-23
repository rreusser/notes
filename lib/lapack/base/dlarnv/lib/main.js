'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarnv = require( './dlarnv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarnv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarnv;
