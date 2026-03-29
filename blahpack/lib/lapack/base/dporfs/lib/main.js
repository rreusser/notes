'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dporfs = require( './dporfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dporfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dporfs;
