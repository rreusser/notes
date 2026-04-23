'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgerfs = require( './zgerfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgerfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgerfs;
