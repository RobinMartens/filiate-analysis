% Express the similarities between the texts as dendrograms
function build_dendrogram(data, names, fig)

    % normalize
    %data = data / max(max(data));

    figure(fig);
    dendrogram(linkage(squareform(data)), 'labels', names);
    title('Similarities of Chapter 1');
    xlabel('source');
    ylabel('relative similarity');
    

end