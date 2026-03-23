

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlartg = require( './dlartg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlartg, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlartg;
